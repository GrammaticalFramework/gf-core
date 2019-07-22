#include <napi.h>

// #include <pgf/pgf.h>
// #include <gu/file.h>
// #include <gu/variant.h>
// #include <gu/map.h>
// #include <gu/enum.h>
// #include <gu/exn.h>
// #include <pgf/literals.h>
// #include <pgf/linearizer.h>

Napi::Value readPGF(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();

  if (info.Length() < 1) {
    Napi::TypeError::New(env, "Wrong number of arguments").ThrowAsJavaScriptException();
    return env.Null();
  }

  return info[0].As<Napi::String>();
}

Napi::Object initAll(Napi::Env env, Napi::Object exports) {
  exports.Set("readPGF", Napi::Function::New(env, readPGF));
  return exports;
}

NODE_API_MODULE(pgf, initAll)
