#pragma once

#include "tesl_common.hpp"
#include "tesl_symbol.hpp"
#include "tesl_symbol_table.hpp"

namespace tesl {
  struct TypeInfoData {
    GlobalNameSymbol local_name_symbol;
    GlobalNameSymbol global_name_symbol;
    SizeT size;
    SizeT align;

    // initializes the data at the return address (no args)
    FnPtrBare _init;

    // copies the argument into the return address
    FnPtrBare _copy_init;

    // moves the argument into the return address
    FnPtrBare _move_init;

    // deinitializes the argument (no return)
    FnPtrBare _deinit;
  };

  struct TypeInfo : TypeInfoData {
    InstanceSymbolTable<Name, MemberVariable> _member_variables;
    InstanceSymbolTable<Signature, MemberFunction> _member_functions;
    HashT _hash;
  
    void format(FmtParseContext & parse_ctx, FmtFormatContext & ctx) const;
    Result<Null, CallError> coerce(Type type, void * this_, void * ret) const;
    Result<Null, CallError> call_method(GlobalTypeSymbol type_symbol, Signature signature, void * this_, void * args, void * ret) const;
    bool has_method(Signature signature) const;

    Optional<const MemberVariable &> get_member_variable(Name name) const;
    Optional<const MemberFunction &> get_member_function(Signature signature) const;

    TESL_ALWAYS_INLINE WidePtr allocate() const {
      return {operator new(size, std::align_val_t{static_cast<size_t>(align)}), size};
    }

    TESL_ALWAYS_INLINE void deallocate(WidePtr ptr) const {
      operator delete(ptr.ptr);
    }

    TESL_ALWAYS_INLINE void init(void * ptr) const {
      _init(ptr, nullptr, nullptr);
    }

    TESL_ALWAYS_INLINE void copy_init(void * ptr, void * from) const {
      _copy_init(ptr, from, nullptr);
    }

    TESL_ALWAYS_INLINE void move_init(void * ptr, void * from) const {
      _move_init(ptr, from, nullptr);
    }

    TESL_ALWAYS_INLINE void deinit(void * ptr) const {
      _deinit(ptr, nullptr, nullptr);
    }

    TESL_ALWAYS_INLINE WidePtr new_() const {
      WidePtr ret = allocate();
      init(ret.ptr);
      return ret;
    }

    TESL_ALWAYS_INLINE void delete_(WidePtr wptr) const {
      deinit(wptr.ptr);
      deallocate(wptr);
    }

    TESL_ALWAYS_INLINE bool operator==(const TypeInfo & other) const { return this == &other; }
    TESL_ALWAYS_INLINE bool operator!=(const TypeInfo & other) const { return !this->operator==(other); }

    TESL_ALWAYS_INLINE friend HashT hash(const TypeInfo & v) { return v._hash; }

    TypeInfo & operator=(const TypeInfo &) = default;
    TypeInfo & operator=(TypeInfo &&) = default;

    TypeInfo(const TypeInfo &) = default;
    TypeInfo(TypeInfo &&) = default;
  
    TypeInfo(TypeInfoData p_type_info_data);
    TypeInfo();
    ~TypeInfo();
  };

  struct Type {
    GlobalTypeSymbol symbol;
    //bool is_ref = false;
    HashT _hash;

    TypeInfo & get_data() const;
    bool is_valid() const;

    void format(FmtParseContext & parse_ctx, FmtFormatContext & ctx) const;
    Result<Null, CallError> call_method(Signature signature, void * this_, void * args, void * ret) const;

    Optional<const MemberVariable &> get_member_variable(Name name) const;
    Optional<const MemberFunction &> get_member_function(Signature signature) const;

    Name local_name() const;
    Name global_name() const;

    SizeT size() const;
    SizeT align() const;

    WidePtr allocate() const;
    void deallocate(WidePtr ptr) const;

    void init(void * ptr) const;
    void deinit(void * ptr) const;

    void copy_init(void * ptr, void * from) const;
    void move_init(void * ptr, void * from) const;

    WidePtr new_() const;
    void delete_(WidePtr ptr) const;

    Type & operator=(const Type & other) = default;
    Type & operator=(Type && other) = default;

    TESL_ALWAYS_INLINE bool operator==(const Type & other) const {
      return symbol == other.symbol;// && is_ref == other.is_ref;
    }

    TESL_ALWAYS_INLINE bool operator!=(const Type & other) const { return !this->operator==(other); }

    TESL_ALWAYS_INLINE friend HashT hash(const Type & v) { return v._hash; }

    TESL_ALWAYS_INLINE friend const TypeInfo & format_as(const Type & v) { return v.get_data(); }

    static Type make_value(GlobalTypeSymbol s) { return {s, false}; }
    //static Type make_ref(GlobalTypeSymbol s) { return {s, true}; }
    
    Type(const Type & other) = default;
    Type(Type && other) = default;

    Type(GlobalTypeSymbol s, bool p_is_ref);
    Type();
  };

  template<typename T>
  Type get_type_of() {
    static_assert(false, "unknown type");
    return {};
  }

#define TESL_BUILTIN_TYPE_DEF(type, local_name, global_name) \
  template<> Type get_type_of<type>();
#include "tesl_builtin_types.inc"

}
