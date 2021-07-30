#ifndef VARIANT_H_
#define VARIANT_H_

typedef uintptr_t variant;

template<class V>
variant variant_close(ref<V> r, uint8_t tag)
{
    return (((moffset) r) | tag);
}

#endif /* VARIANT_H_ */
