#ifndef PYPGF_COMPAT_H_
#define PYPGF_COMPAT_H_

#if PY_MAJOR_VERSION >= 3
    // #define PyIntObject                  PyLongObject
    // #define PyInt_Type                   PyLong_Type
    // #define PyInt_Check(op)              PyLong_Check(op)
    // #define PyInt_CheckExact(op)         PyLong_CheckExact(op)
    // #define PyInt_FromString             PyLong_FromString
    // #define PyInt_FromUnicode            PyLong_FromUnicode
    // #define PyInt_FromLong               PyLong_FromLong
    // #define PyInt_FromSize_t             PyLong_FromSize_t
    // #define PyInt_FromSsize_t            PyLong_FromSsize_t
    // #define PyInt_AsLong                 PyLong_AsLong
    // #define PyInt_AS_LONG                PyLong_AS_LONG
    // #define PyInt_AsSsize_t              PyLong_AsSsize_t
    // #define PyInt_AsUnsignedLongMask     PyLong_AsUnsignedLongMask
    // #define PyInt_AsUnsignedLongLongMask PyLong_AsUnsignedLongLongMask

    #define PyStringObject               PyUnicodeObject
    // #define PyString_Check               PyUnicode_Check
    #define PyString_FromStringAndSize   PyUnicode_FromStringAndSize
    // #define PyString_FromFormat          PyUnicode_FromFormat
    // #define PyString_Concat(ps,s)        {PyObject* tmp = *(ps); *(ps) = PyUnicode_Concat(tmp,s); Py_DECREF(tmp);}
    #define PyString_Compare             PyUnicode_Compare
#endif

#endif // PYPGF_COMPAT_H_
