#pragma once 

#include "Log.h"

#include <memory>

namespace clear {

    template<typename Type>
    class Ref
    {
    public:
        Ref() = default;
        Ref(Type* ptr, size_t* refCount)
            : m_Ptr(ptr), m_RefCount(refCount)
        {
            CLEAR_ASSERT(m_Ptr && m_RefCount, "m_Ptr/m_RefCount assigned to null");
            Incref();
        }
        ~Ref()
        {
            Decref();
        }

        template<typename OtherType>
        Ref(const Ref<OtherType>& other)
            : m_Ptr(other.Get()), m_RefCount(other.GetRefCount())
        {
            Incref();
        }

        Ref(const Ref& other)
            : m_Ptr(other.m_Ptr), m_RefCount(other.m_RefCount)
        {
            Incref();
        }

        Ref& operator=(const Ref& other)
        {
            if (this != &other)
            {
                Decref();

                m_Ptr = other.m_Ptr;
                m_RefCount = other.m_RefCount;

                Incref();
            }

            return *this;
        }

        Ref(Ref&& other) noexcept
            : m_Ptr(other.m_Ptr), m_RefCount(other.m_RefCount)
        {
            other.m_Ptr = nullptr;
            other.m_RefCount = nullptr;
        }

        Ref& operator=(Ref&& other) noexcept
        {
            if (this != &other)
            {
                Decref();

                m_Ptr = other.m_Ptr;
                m_RefCount = other.m_RefCount;
                other.m_Ptr = nullptr;
                other.m_RefCount = nullptr;
            }

            return *this;
        }

        inline Type* Get() const { return m_Ptr; }
        inline size_t GetCount() const { return *m_RefCount; }
        inline size_t* GetRefCount() const { return m_RefCount; }

        inline void Incref()
        {
            CLEAR_ASSERT(m_RefCount, "ref count was null");
            (*m_RefCount)++;
        }
        inline void Decref()
        {
            if (!m_Ptr)
            {
                CLEAR_ASSERT(!m_RefCount, "ref count wasn't deleted with ptr");
                return;
            }

            if (--(*m_RefCount) == 0)
            {
                delete m_Ptr;
                delete m_RefCount;

                m_Ptr = nullptr;
                m_RefCount = nullptr;
            }

        }
        inline void Reset()
        {
            Decref();
        }

        inline Type& operator*() const { return *Get(); }
        inline Type* operator->() const { return Get(); }

        inline operator bool() const { return Get() != nullptr; }

        template<typename ...Args>
        static Ref<Type> Create(Args&&... args)
        {
            Type* object = new Type(std::forward<Args>(args)...);
            size_t* refCount = new size_t(0);

            return Ref<Type>(object, refCount);
        }
        
        template<typename To, typename From>
        static Ref<To> DynamicCast(const Ref<From>& other)
        {
            const auto ptr = dynamic_cast<To*>(other.Get());

            if (ptr)
                return Ref<To>(ptr, other.GetRefCount());

            return {};
        }

    private:
        Type* m_Ptr = nullptr;
        size_t* m_RefCount = nullptr;
    };

}