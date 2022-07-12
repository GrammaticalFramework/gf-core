#ifndef HEAP_H
#define HEAP_H

template <class A>
class PGF_INTERNAL_DECL Heap {
public:
    Heap() {
        len   = 0;
        avail = 0;
        values = NULL;
    }

    ~Heap() { free(values); }

    void push(A value) {
        if (len >= avail) {
            avail = get_next_padovan(len+1);
            A *new_values = (A *) realloc(values, sizeof(A)*avail);
            if (new_values == NULL)
                throw pgf_systemerror(errno);
            values = new_values;
        }
        siftdown(value, 0, len);
        len++;
    }

    bool is_empty() { return (len == 0); }
    
    A top() { return values[0]; }

    A pop() {
        A top = values[0];
        siftup(&values[len-1],0);
        len--;
        return top;
    }

private:
    size_t len;
    size_t avail;
    A *values;

    void siftdown(A value, size_t startpos, size_t pos) {
        while (pos > startpos) {
            size_t parentpos = (pos - 1) >> 1;
            A parent = values[parentpos];

            if (value >= parent)
                break;

            values[pos] = parent;
            pos = parentpos;
        }

        values[pos] = value;
    }
    
    void siftup(A *pvalue, size_t pos) {
        size_t startpos = pos;
        size_t endpos   = len;

        size_t childpos = 2*pos + 1;
        while (childpos < endpos) {
            size_t rightpos = childpos + 1;
            if (rightpos < endpos &&
                values[childpos] >= values[rightpos]) {
                childpos = rightpos;
            }

            values[pos] = values[childpos];
            pos = childpos;
            childpos = 2*pos + 1;
       }
       
       siftdown(*pvalue, startpos, pos);
    }
};

#endif
