#include "llvm/IR/PassManager.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/Support/RecyclingAllocator.h"
#include "llvm/ADT/DenseMapInfo.h"
#include <map>

using namespace llvm;

#define DEBUG_TYPE "tls"

namespace {

std::set<StringRef> EEPoints = {
    "__sysv_signal", "sysv_signal", "signal", "ssignal", "sigset",
    "atexit", "at_quick_exit", "on_exit", "bsearch", "qsort", "qsort_r",
    "clone", "pthread_create", "pthread_once", "pthread_key_create",
    "pthread_atfork", "scandir", "scandir64", "scandirat", "scandirat64",
    "_obstack_begin", "_obstack_begin_1", "glob", "glob64",
};

std::set<StringRef> TLSPrefix = {
    "RBX","RAX","RCX","RDX","RSP","RBP","RSI","RDI","RIP",
    "R8","R9","R10","R11","R12","R13","R14","R15",
    "CF","PF","ZF","SF","OF","AF",
    // "TF","IF",
    // "XMM0","XMM1","XMM2",
    // "XMM3","XMM4","XMM5","XMM6","XMM7","XMM8","XMM9",
};

class SimpleValue {
public:
    virtual ~SimpleValue() = 0;
    virtual std::string getName() = 0;
    virtual Value* getValue() = 0;
    virtual Value* getTLS() = 0;
    // static std::string getName(Value *TLS) {
    //     if(GPRValue::isGPR(TLS)) return GPRValue::getName(TLS);
    //     if(FlagValue::isFlag(TLS)) return FlagValue::getName(TLS);
    //     if(XMMValue::isXMM(TLS)) return XMMValue::getName(TLS);
    // }
    virtual void createStore(IRBuilder<> &Builder) = 0;
    virtual void createLoad(IRBuilder<> &Builder) = 0;
    virtual void createAlloca(IRBuilder<> &Builder) = 0;
    virtual SimpleValue *get() { return this; }
};

class GEPValue:public SimpleValue {
public:
    GEPValue(GetElementPtrInst *Inst,SimpleValue *XMMBase,Value *slot):GEPInst(Inst),XMMBase(XMMBase),tls_slot(slot) {}
    void createStore(IRBuilder<> &Builder) override { }
    void createLoad(IRBuilder<> &Builder) override { }
    void createAlloca(IRBuilder<> &Builder) override { }
    std::string getName() override { };
    Value* getValue() override { return GEPInst; };
    Value* getTLS() override { return tls_slot; };
    SimpleValue *get() override { errs()<<"gep get\n"; return XMMBase->get(); }
private:
    GetElementPtrInst *GEPInst;
    SimpleValue *XMMBase;
    Value *tls_slot;
};

class GPRValue:public SimpleValue {
public:
    static bool isGPR(Value *value) {
        std::string TLSName = std::string(value->getName());
        if(TLSName.find('_') == std::string::npos) {
            return false;
        }
        std::string Name = TLSName.substr(0,TLSName.find('_'));
        if(TLSPrefix.count(Name) == 0) return false;
        if(Name.back() == 'F' || Name[0] == 'X') {
            return false;
        }
        return true;
    }

    static std::string getName(Value *TLS) {
        std::string TLSName = TLS->getName().str();
        std::string Name = TLSName.substr(0,TLSName.find('_'));
        return "Local_" + Name;
    }

    GPRValue(Value *TLS,Function &F):TLS(TLS) {
        // StringRef TLSName = TLS->getName();
        // Name = TLSName.substr(0,TLSName.find('_'));
        Name = getName(TLS);
        auto Ty32 = Type::getInt32Ty(F.getContext());
        auto Ty64 = Type::getInt64Ty(F.getContext());
        Alloca = new AllocaInst(Ty64,F.getAddressSpace(),ConstantInt::get(Ty32,1),Align(8),Name);
        // Alloca = new AllocaInst(Ty32,F.getAddressSpace(),ConstantInt::get(Ty32,2),Align(8),Name);
    }

    ~GPRValue() = default;

    std::string getName() override { return Name; }
    Value* getValue() override { return Alloca; }
    Value* getTLS() override { return TLS; }

    void createStore(IRBuilder<> &Builder) override {
        Value *Temp = Builder.CreateLoad(Builder.getInt64Ty(),Alloca,"Temp");
        Builder.CreateStore(Temp,TLS);
    }

    void createLoad(IRBuilder<> &Builder) override {
        Value *Temp = Builder.CreateLoad(Builder.getInt64Ty(),TLS,"Temp");
        Builder.CreateStore(Temp,Alloca);
    }

    void createAlloca(IRBuilder<> &Builder) override {
        Builder.Insert(Alloca,Name);
    }

private:
    Value *Alloca;
    Value *TLS;
    std::string Name;
};

class FlagValue:public SimpleValue {
public:
    static bool isFlag(Value *tls) {
        std::string TLSName = std::string(tls->getName());
        if(TLSName.find('_') == std::string::npos) {
            return false;
        }
        std::string Name = TLSName.substr(0,TLSName.find('_'));
        if(TLSPrefix.count(Name) == 0) return false;
        if(Name.back() != 'F') {
            return false;
        }
        return true;
    }

    static std::string getName(Value *TLS) {
        std::string TLSName = TLS->getName().str();
        std::string Name = TLSName.substr(0,TLSName.find('_'));
        return "Local_" + Name;
    }

    FlagValue(Value *TLS,Function &F):TLS(TLS) {
        // StringRef TLSName = TLS->getName();
        // Name = TLSName.substr(0,TLSName.find('_'));
        Name = getName(TLS);
        auto Ty8 = Type::getInt8Ty(F.getContext());
        auto Ty32 = Type::getInt32Ty(F.getContext());
        Alloca = new AllocaInst(Ty8,F.getAddressSpace(),ConstantInt::get(Ty32,1),Align(1),Name);
    }

    ~FlagValue() = default;

    std::string getName() override { return Name; }
    Value* getValue() override { return Alloca; }
    Value* getTLS() override { return TLS; }

    void createStore(IRBuilder<> &Builder) override {
        Value *Temp = Builder.CreateLoad(Builder.getInt8Ty(),Alloca,"Temp");
        Builder.CreateStore(Temp,TLS);
    }

    void createLoad(IRBuilder<> &Builder) override {
        Value *Temp = Builder.CreateLoad(Builder.getInt8Ty(),TLS,"Temp");
        Builder.CreateStore(Temp,Alloca);
    }

    void createAlloca(IRBuilder<> &Builder) override {
        Builder.Insert(Alloca,Name);
    }

private:
    Value *Alloca;
    Value *TLS;
    std::string Name;
};
//offset是一个XMM Value相对于寄存器结构体基地址的
//localoffset是相对于本XMM寄存器起始地址的
class XMMValue:public SimpleValue {
public:
    static bool isXMM(Value *tls) {
        std::string TLSName = std::string(tls->getName());
        if(TLSName.find('_') == std::string::npos) {
            return false;
        }
        std::string Name = TLSName.substr(0,TLSName.find('_'));
        if(TLSPrefix.count(Name) == 0 || Name[0] != 'X') {
            return false;
        }
        return true;
    }
    //BaseName of XMM which looks like Local_XMMx
    static std::string getName(Value *TLS) {
        std::string TLSName = TLS->getName().str();
        int size = TLSName.find('_');//获取第一个'_'的位置
        return "Local_" + TLSName.substr(0,size);
    } 
    //SlotName = Local_XMM[x]_[Offset] ignore [ and ]
    static std::string getSlotName(Value *TLS) {
        std::string TLSName = TLS->getName().str();
        int size = TLSName.find('_',TLSName.find('_')+1);//获取第二个'_'的位置
        return "Local_" + TLSName.substr(0,size);
    }

    XMMValue(Value *TLS,Function &F):Entry(&F.getEntryBlock()) {
        Name = XMMValue::getName(TLS);
        Value *regs_base = F.getParent()->getGlobalVariable("__mcsema_reg_state",true);
        int offset = getoffset();
        IntegerType *Ty8 = Type::getInt8Ty(F.getContext());
        IntegerType *Ty32 = Type::getInt32Ty(F.getContext());
        IntegerType *Ty64 = Type::getInt64Ty(F.getContext());
        IntegerType *Ty128 = Type::getInt128Ty(F.getContext());
        XMMBase = GetElementPtrInst::Create(Ty8,regs_base,ConstantInt::get(Ty32,offset),Name+".Base");
        Alloca = new AllocaInst(Ty128,F.getAddressSpace(),ConstantInt::get(Ty32,1),Align(16),Name);
        // Alloca = new AllocaInst(Ty32,F.getAddressSpace(),ConstantInt::get(Ty32,4),Align(16),Name);
    }
    
    ~XMMValue() {
        // for(auto [_,GEP] : slot) {
        //     delete GEP;
        // }
    };

    std::string getName() override { return Name; }
    Value* getValue() override { return Alloca; }
    Value* getTLS() override { return XMMBase; }

    void createStore(IRBuilder<> &Builder) override {
        Value *Temp = Builder.CreateLoad(Builder.getInt128Ty(),Alloca,"Temp");
        Builder.CreateStore(Temp,XMMBase);
    }

    void createLoad(IRBuilder<> &Builder) override {
        Value *Temp = Builder.CreateLoad(Builder.getInt128Ty(),XMMBase,"Temp");
        Builder.CreateStore(Temp,Alloca);
    }

    void createAlloca(IRBuilder<> &Builder) override {
        Builder.Insert(Alloca,Name);
        Builder.Insert(XMMBase,Name+".Base");
        for(auto [SlotName,Slot]:slot) {
            // errs()<<"Alloca create : "<<Slot->getValue()->getName() <<"\n";
            Builder.Insert(Slot->getValue(),SlotName);
        }
    }

    SimpleValue* getSlotFrom(Value *TLS) {
        std::string SlotName = XMMValue::getSlotName(TLS);
        errs() << "Slot : "<<SlotName<<"\n";
        if(slot.count(SlotName)) return slot[SlotName];

        std::string TLSName = TLS->getName().str();
        std::string temp = TLSName.substr(TLSName.find('_')+1);
        int offset = std::stoi(temp.substr(0,temp.find('_')));

        IntegerType *Ty8 = Type::getInt8Ty(TLS->getContext());
        IntegerType *Ty64 = Type::getInt64Ty(TLS->getContext());
        // GetElementPtrInst *Slot = GetElementPtrInst::Create(Ty8,XMMBase,ConstantInt::get(Ty64,offset2local(offset)),SlotName,Entry->getFirstInsertionPt());
        GetElementPtrInst *Slot = GetElementPtrInst::Create(Ty8,Alloca,ConstantInt::get(Ty64,offset2local(offset)),SlotName,nullptr);

        slot[SlotName] = new GEPValue(Slot,this,TLS);
        errs() << "new Slot:"<<Slot->getName() << "\n";
        return slot[SlotName];
    }

private:
    int getoffset() {
        return 16 + (Name.back() - '0') * 64;
    }

    int offset2local(int offset) {
        return offset - getoffset();
    }

private:
    Value *Alloca;
    std::map<std::string,GEPValue*> slot;
    Value *XMMBase; //XMM寄存器的base
    std::string Name;
    BasicBlock *Entry;
};

class newTLSImpl {
public:
    friend class SimpleValueFactory;
    newTLSImpl(Function &F,FunctionAnalysisManager &AM):
    F(F),AM(AM),DT(AM.getResult<DominatorTreeAnalysis>(F)),
    AllocaBuilder(&*F.getEntryBlock().getFirstNonPHIOrDbgOrAlloca()),
    LastBuilder(&*F.getEntryBlock().getFirstNonPHIOrDbgOrAlloca()) {}
    bool ShouldHandle() {
        std::string name = F.getName().str();
        if(name.substr(0,3) == "sub") return true;
        return false;
    }
    bool runOnFunction();
private:
    // using AllocatorTy = RecyclingAllocator<BumpPtrAllocator,ScopedHashTableVal<StringRef,SimpleValue*>>;
    // using ScopedHTType = ScopedHashTable<StringRef,SimpleValue*,DenseMapInfo<StringRef>,AllocatorTy>;

    // class StackNode {
    // public:
    //     friend class newTLSImpl;
    //     StackNode(ScopedHTType &Name2Local,ScopedHTType &Name2TLS,BasicBlock *BB):
    //     Scope_Name2Local(Name2Local), Scope_Name2TLS(Name2TLS),BB(BB) { }
    // private:
    //     BasicBlock *BB;
    //     ScopedHTType::ScopeTy Scope_Name2Local;
    //     ScopedHTType::ScopeTy Scope_Name2TLS;
    // };
    // bool process(StackNode *node);

    
    bool isTarget(Value *v) {
        return GPRValue::isGPR(v) || FlagValue::isFlag(v) || XMMValue::isXMM(v);
    }


private:
    Function &F;
    DominatorTree &DT;
    FunctionAnalysisManager &AM;
    // TLSImpl oldImpl;
    StringMap<SimpleValue*> Name2Local;
    StringMap<SimpleValue*> Name2TLS;
    // ScopedHTType table;
    IRBuilder<> AllocaBuilder;
    IRBuilder<> LastBuilder;
};

class SimpleValueFactory {
public:
    SimpleValueFactory(newTLSImpl *impl):impl(impl) { };

    SimpleValue *newSimpleValue(Value *tls) {
        // StringMap<SimpleValue*> &Name2Local = impl->Name2Local;
        // StringMap<SimpleValue*> &Name2TLS = impl->Name2TLS;
        if(XMMValue::isXMM(tls)) {
            errs()<<"create XMM "<<XMMValue::getName(tls)<<"\n";
            if(Total.count(XMMValue::getName(tls)) == 0) {
            errs()<<"create new XMM "<<XMMValue::getName(tls)<<"\n";
                Total[XMMValue::getName(tls)] = new XMMValue(tls,impl->F);
            }
            count[XMMValue::getName(tls)] ++;
            return static_cast<XMMValue*>(Total[XMMValue::getName(tls)])->getSlotFrom(tls);
        }
        SimpleValue *v;
        if(GPRValue::isGPR(tls)) {
            errs()<<"create GPR "<<GPRValue::getName(tls)<<"\n";
            if(Total.count(GPRValue::getName(tls)) == 0) {
                Total[GPRValue::getName(tls)] = new GPRValue(tls,impl->F);
            }
            v = Total[GPRValue::getName(tls)];
            count[GPRValue::getName(tls)] ++;
        } else if(FlagValue::isFlag(tls)) {
            errs()<<"create Flag "<<FlagValue::getName(tls)<<"\n";
            if(Total.count(FlagValue::getName(tls)) == 0) {
                Total[GPRValue::getName(tls)] = new FlagValue(tls,impl->F);
            }
            v = Total[GPRValue::getName(tls)];
            count[GPRValue::getName(tls)] ++;
        }
        return v;
    }

    void createAlloca(IRBuilder<> &Builder) {
        for(auto [Name,SV] : Total) {
            // if(count.count(Name) <= 1) continue;
            Builder.SetInsertPointPastAllocas(&impl->F);
            SV->createAlloca(Builder);
        }
    }

    void createAlloca(IRBuilder<> &Builder,std::set<SimpleValue*> SValues) {
        for(auto SV : SValues) {
            Builder.SetInsertPointPastAllocas(&impl->F);
            SV->createAlloca(Builder);
        }
    }

private:
    newTLSImpl *impl;
    std::map<std::string,SimpleValue *> Total;
    std::map<std::string,int> count;
};

// bool newTLSImpl::process(StackNode *node) {
//     BasicBlock *BB = node->BB;

// }

bool SkipFunction(const StringRef &Fname) {
    bool skip = false;
    {
        StringRef subname = Fname.substr(0,4);
        if(subname == "ext_" || subname == "sub_") {
            skip |= true;
        }
    }
    if(Fname == "__mcsema_init_reg_state") {
        skip |= true;
    }
    {
        if(EEPoints.count(Fname)) {
            skip |= true;
        }
    }
    return skip;
}

bool newTLSImpl::runOnFunction() {
    SimpleValueFactory factory(this);
    std::set<SimpleValue *> Values;
    std::map<SimpleValue*,Instruction*> map;
    for(BasicBlock &BB : F) {
        LastBuilder.SetInsertPoint(BB.getFirstInsertionPt());
        for(Instruction &Inst : make_early_inc_range(BB)) {
            for(int i = 0; i < Inst.getNumOperands(); i++) {
                Value *V = Inst.getOperand(i);
                // if(V->hasName()) {
                //     errs()<<V->getName()<<"\n";
                // }
                if(isTarget(V)) {
                    errs()<<"is target : "<<V->getName()<<"\n";
                    SimpleValue *SV = factory.newSimpleValue(V);
                    if(map.count(SV) == 0) {
                        map[SV] = &Inst;
                        continue;
                    }
                    Values.insert(SV->get());
                    Inst.setOperand(i,SV->getValue());
                    if(map[SV] != nullptr) {
                        for(int j = 0; j < map[SV]->getNumOperands(); j++) {
                            Value *v = map[SV]->getOperand(j);
                            if(isTarget(v)) {
                                SimpleValue *sv = factory.newSimpleValue(v);
                                if(sv == SV) {
                                    map[SV]->setOperand(j,sv->getValue());
                                }
                            }
                        }
                        // map[SV]->replaceUsesOfWith(SV->getTLS(),SV->getValue());
                        map[SV] = nullptr;
                    }
                }
            }
            if(CallInst *CI = dyn_cast<CallInst>(&Inst)) {
                Function *Callee = CI->getCalledFunction();
                //这些call指令并不影响TLS，基本都是内置函数
                if(Callee && Callee->hasName() && 
                    ( Callee->isIntrinsic())) {
                    continue;
                }
                for(SimpleValue *SV : Values) {
                    errs()<<"Load "<<SV->getName()<<"\n";
                    SV->createLoad(LastBuilder);
                }
                LastBuilder.SetInsertPoint(&Inst);
                for(SimpleValue *SV : Values) {
                    errs()<<"Store "<<SV->getName()<<"\n";
                    SV->createStore(LastBuilder);
                }
                LastBuilder.SetInsertPoint(Inst.getNextNode());
                Values.clear();
                map.clear();
            }            
        }
        errs()<<"values size " << Values.size() << "\n";
        for(SimpleValue *SV : Values) {
            errs()<<"Load "<<SV->getName()<<"\n";
            SV->createLoad(LastBuilder);
        }
        LastBuilder.SetInsertPoint(&BB.back());
        for(SimpleValue *SV : Values) {
            errs()<<"Store "<<SV->getName()<<"\n";
            SV->createStore(LastBuilder);
        }
        Values.clear();
        map.clear();
    }
    AllocaBuilder.SetInsertPoint(&*F.getEntryBlock().getFirstInsertionPt());
    factory.createAlloca(AllocaBuilder);
    return true;
}

}



namespace {
class TLSIRPass : public PassInfoMixin<TLSIRPass> {
public:
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM) {

        newTLSImpl impl(F,AM);
        bool Changed = false;
        if(!impl.ShouldHandle()) {
            errs()<<"Skip Function : "<<F.getName()<<"\n";
            return PreservedAnalyses::all();
        }
        errs()<<"--------- Handle Function Begin : "<<F.getName()<<" ---------\n";
        Changed != impl.runOnFunction();
        errs()<<"--------- Handle Function End : "<<F.getName()<<" ---------\n";

        if(Changed) {
            return PreservedAnalyses::none();
        } else {
            return PreservedAnalyses::all();
        }
    }
};
}


void RegisterCB(PassBuilder &PB) {
    PB.registerPipelineParsingCallback(
        [](StringRef Name, FunctionPassManager &FPM, 
                ArrayRef<PassBuilder::PipelineElement>) {
            if (Name == "tls") {
                FPM.addPass(TLSIRPass());
                return true;
            }
            return false;
        }
    );
    // PB.registerVectorizerStartEPCallback(
    //     [](FunctionPassManager &FPM, OptimizationLevel Level) {
    //         FPM.addPass(TLSIRPass());
    //     }
    // );
}

PassPluginLibraryInfo getTLSPluginInfo() {
    return {LLVM_PLUGIN_API_VERSION, "TLS", "v1", RegisterCB};
}

#ifndef LLVM_TLS_LINK_INTO_TOOLS
extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
    return getTLSPluginInfo();
}
#endif