#ifndef MD5_H
#define MD5_H

struct PGF_INTERNAL_DECL MD5Digest {
    uint8_t b[16];
};

inline bool operator < (const MD5Digest &d1, const MD5Digest &d2) {
    return memcmp(d1.b, d2.b, 16) < 0;
}

class PGF_INTERNAL_DECL MD5Context {
    uint64_t size;        // Size of input in bytes
    uint32_t buffer[4];   // Current accumulation of hash
    uint8_t input[64];    // Input to be used in the next step

public:
    MD5Context();
    void update(uint8_t *input, size_t input_len);

    template <class T>
    void update(T &input)
    {
        update((uint8_t *) &input, sizeof(T));
    }

    void finalize(MD5Digest *digest);
};

#endif
