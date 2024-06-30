#!/usr/bin/env node

if (parseInt(process.versions.node.split('.')[0]) < 20) {
  throw new Error("This program requires Node v20 or later to run")
}

try {
(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

var $gren_lang$node$Node$InitDone = function (a) {
	return { $: 'InitDone', a: a };
};
var $gren_lang$node$Node$Uninitialized = { $: 'Uninitialized' };


// TASKS

function _Scheduler_succeed(value) {
  return {
    $: 0,
    a: value,
  };
}

function _Scheduler_fail(error) {
  return {
    $: 1,
    a: error,
  };
}

function _Scheduler_binding(callback) {
  return {
    $: 2,
    b: callback,
    c: null,
  };
}

var _Scheduler_andThen = F2(function (callback, task) {
  return {
    $: 3,
    b: callback,
    d: task,
  };
});

var _Scheduler_onError = F2(function (callback, task) {
  return {
    $: 4,
    b: callback,
    d: task,
  };
});

function _Scheduler_receive(callback) {
  return {
    $: 5,
    b: callback,
  };
}

// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task) {
  var proc = {
    $: 0,
    e: _Scheduler_guid++,
    f: task,
    g: null,
    h: [],
  };

  _Scheduler_enqueue(proc);

  return proc;
}

function _Scheduler_spawn(task) {
  return _Scheduler_binding(function (callback) {
    callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
  });
}

function _Scheduler_rawSend(proc, msg) {
  proc.h.push(msg);
  _Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function (proc, msg) {
  return _Scheduler_binding(function (callback) {
    _Scheduler_rawSend(proc, msg);
    callback(_Scheduler_succeed({}));
  });
});

function _Scheduler_kill(proc) {
  return _Scheduler_binding(function (callback) {
    var task = proc.f;
    if (task && task.$ === 2 && task.c) {
      task.c();
    }

    proc.f = null;

    callback(_Scheduler_succeed({}));
  });
}

/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/

var _Scheduler_working = false;
var _Scheduler_queue = [];

function _Scheduler_enqueue(proc) {
  _Scheduler_queue.push(proc);
  if (_Scheduler_working) {
    return;
  }
  _Scheduler_working = true;
  while ((proc = _Scheduler_queue.shift())) {
    _Scheduler_step(proc);
  }
  _Scheduler_working = false;
}

function _Scheduler_step(proc) {
  while (proc.f) {
    var rootTag = proc.f.$;
    if (rootTag === 0 || rootTag === 1) {
      while (proc.g && proc.g.$ !== rootTag) {
        proc.g = proc.g.i;
      }
      if (!proc.g) {
        return;
      }
      proc.f = proc.g.b(proc.f.a);
      proc.g = proc.g.i;
    } else if (rootTag === 2) {
      proc.f.c = proc.f.b(function (newRoot) {
        proc.f = newRoot;
        _Scheduler_enqueue(proc);
      });
      return;
    } else if (rootTag === 5) {
      if (proc.h.length === 0) {
        return;
      }
      proc.f = proc.f.b(proc.h.shift());
    } // if (rootTag === 3 || rootTag === 4)
    else {
      proc.g = {
        $: rootTag === 3 ? 0 : 1,
        b: proc.f.b,
        i: proc.g,
      };
      proc.f = proc.f.d;
    }
  }
}
var $gren_lang$core$Task$andThen = _Scheduler_andThen;
var $gren_lang$core$Basics$apL$ = function(f, x) {
	return f(x);
};
var $gren_lang$core$Basics$apL = F2($gren_lang$core$Basics$apL$);
var $gren_lang$core$Basics$apR$ = function(x, f) {
	return f(x);
};
var $gren_lang$core$Basics$apR = F2($gren_lang$core$Basics$apR$);


var process = require("node:process");

var _Node_log = F2(function (text, args) {
  // This function is used for simple applications where the main function returns String
  // NOTE: this function needs _Platform_export available to work
  console.log(text);
  return {};
});

var _Node_init = _Scheduler_binding(function (callback) {
  callback(
    _Scheduler_succeed({
      applicationPath: _FilePath_fromString(module.filename),
      arch: process.arch,
      args: process.argv,
      platform: process.platform,
      stderr: process.stderr,
      stdin: process.stdin,
      stdout: process.stdout,
    })
  );
});

var _Node_getEnvironmentVariables = _Scheduler_binding(function (callback) {
  callback(_Scheduler_succeed(_Node_objToDict(process.env)));
});

var _Node_exitWithCode = function (code) {
  return _Scheduler_binding(function (callback) {
    process.exit(code);
  });
};

var _Node_setExitCode = function (code) {
  return _Scheduler_binding(function (callback) {
    process.exitCode = code;
    callback(_Scheduler_succeed({}));
  });
};

// Helpers

function _Node_objToDict(obj) {
  var dict = $gren_lang$core$Dict$empty;

  for (var key in obj) {
    dict = A3($gren_lang$core$Dict$set, key, obj[key], dict);
  }

  return dict;
}


var path = require("node:path");
var process = require("node:process");

var _FilePath_fromPosix = function (str) {
  return _FilePath_parse(path.posix, str);
};

var _FilePath_fromWin32 = function (str) {
  return _FilePath_parse(path.win32, str);
};

var _FilePath_fromString = function (str) {
  return _FilePath_parse(path, str);
};

var _FilePath_parse = function (pathMod, str) {
  const result = pathMod.parse(pathMod.normalize(str));

  const root = result.root;
  const dirStr = result.dir.startsWith(root)
    ? result.dir.substring(root.length)
    : result.dir;

  const filename =
    result.name === "." && result.ext.length === 0 ? "" : result.name;

  return {
    directory: dirStr === "" ? [] : dirStr.split(pathMod.sep),
    extension: result.ext.length > 0 ? result.ext.substring(1) : "",
    filename: filename,
    root: result.root,
  };
};

var _FilePath_toPosix = function (filePath) {
  if (_FilePath_isEmpty(filePath)) {
    return ".";
  }

  if (filePath.root !== "" && filePath.root !== "/") {
    filePath = { ...filePath, root: "/" };
  }

  return _FilePath_format(path.posix, filePath);
};

var _FilePath_toWin32 = function (filePath) {
  if (_FilePath_isEmpty(filePath)) {
    return ".";
  }

  return _FilePath_format(path.win32, filePath);
};

var _FilePath_toString = function (filePath) {
  if (process.platform.toLowerCase() === "win32") {
    return _FilePath_toWin32(filePath);
  }

  return _FilePath_toPosix(filePath);
};

var _FilePath_isEmpty = function (filePath) {
  return (
    filePath.root === "" &&
    filePath.directory.length === 0 &&
    filePath.filename === "" &&
    filePath.extension === ""
  );
};

var _FilePath_format = function (pathMod, filePath) {
  const filename =
    filePath.extension.length > 0
      ? filePath.filename + "." + filePath.extension
      : filePath.filename;

  let pathArray = null;
  if (filename === "") {
    pathArray = filePath.directory;
  } else {
    pathArray = filePath.directory.concat(filename);
  }

  return filePath.root + pathArray.join(pathMod.sep);
};


// PROGRAMS

var _Platform_worker = F4(function (impl, flagDecoder, debugMetadata, args) {
  return _Platform_initialize(
    flagDecoder,
    args,
    impl.init,
    impl.update,
    impl.subscriptions,
    function () {
      return function () {};
    }
  );
});

// INITIALIZE A PROGRAM

function _Platform_initialize(
  flagDecoder,
  args,
  init,
  update,
  subscriptions,
  stepperBuilder
) {
  var result = A2(
    _Json_run,
    flagDecoder,
    _Json_wrap(args ? args["flags"] : undefined)
  );
  $gren_lang$core$Result$isOk(result) ||
    _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
  var managers = {};
  var initPair = init(result.a);
  var model = initPair.model;
  var stepper = stepperBuilder(sendToApp, model);
  var ports = _Platform_setupEffects(managers, sendToApp);

  function sendToApp(msg, viewMetadata) {
    var pair = A2(update, msg, model);
    stepper((model = pair.model), viewMetadata);
    _Platform_enqueueEffects(managers, pair.command, subscriptions(model));
  }

  _Platform_enqueueEffects(managers, initPair.command, subscriptions(model));

  return ports ? { ports: ports } : {};
}

// TRACK PRELOADS
//
// This is used by code in gren/browser and gren/http
// to register any HTTP requests that are triggered by init.
//

var _Platform_preload;

function _Platform_registerPreload(url) {
  _Platform_preload.add(url);
}

// EFFECT MANAGERS

var _Platform_effectManagers = {};

function _Platform_setupEffects(managers, sendToApp) {
  var ports;

  // setup all necessary effect managers
  for (var key in _Platform_effectManagers) {
    var manager = _Platform_effectManagers[key];

    if (manager.a) {
      ports = ports || {};
      ports[key] = manager.a(key, sendToApp);
    }

    managers[key] = _Platform_instantiateManager(manager, sendToApp);
  }

  return ports;
}

function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap) {
  return {
    b: init,
    c: onEffects,
    d: onSelfMsg,
    e: cmdMap,
    f: subMap,
  };
}

function _Platform_instantiateManager(info, sendToApp) {
  var router = {
    g: sendToApp,
    h: undefined,
  };

  var onEffects = info.c;
  var onSelfMsg = info.d;
  var cmdMap = info.e;
  var subMap = info.f;

  function loop(state) {
    return A2(
      _Scheduler_andThen,
      loop,
      _Scheduler_receive(function (msg) {
        var value = msg.a;

        if (msg.$ === 0) {
          return A3(onSelfMsg, router, value, state);
        }

        return cmdMap && subMap
          ? A4(onEffects, router, value.i, value.j, state)
          : A3(onEffects, router, cmdMap ? value.i : value.j, state);
      })
    );
  }

  return (router.h = _Scheduler_rawSpawn(
    A2(_Scheduler_andThen, loop, info.b)
  ));
}

// ROUTING

var _Platform_sendToApp = F2(function (router, msg) {
  return _Scheduler_binding(function (callback) {
    router.g(msg);
    callback(_Scheduler_succeed({}));
  });
});

var _Platform_sendToSelf = F2(function (router, msg) {
  return A2(_Scheduler_send, router.h, {
    $: 0,
    a: msg,
  });
});

// BAGS

function _Platform_leaf(home) {
  return function (value) {
    return {
      $: 1,
      k: home,
      l: value,
    };
  };
}

function _Platform_batch(list) {
  return {
    $: 2,
    m: list,
  };
}

var _Platform_map = F2(function (tagger, bag) {
  return {
    $: 3,
    n: tagger,
    o: bag,
  };
});

// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/gren/core/issues/980
//   https://github.com/gren/core/pull/981
//   https://github.com/gren/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.

// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;

function _Platform_enqueueEffects(managers, cmdBag, subBag) {
  _Platform_effectsQueue.push({
    p: managers,
    q: cmdBag,
    r: subBag,
  });

  if (_Platform_effectsActive) return;

  _Platform_effectsActive = true;
  for (var fx; (fx = _Platform_effectsQueue.shift()); ) {
    _Platform_dispatchEffects(fx.p, fx.q, fx.r);
  }
  _Platform_effectsActive = false;
}

function _Platform_dispatchEffects(managers, cmdBag, subBag) {
  var effectsDict = {};
  _Platform_gatherEffects(true, cmdBag, effectsDict, null);
  _Platform_gatherEffects(false, subBag, effectsDict, null);

  for (var home in managers) {
    _Scheduler_rawSend(managers[home], {
      $: "fx",
      a: effectsDict[home] || { i: [], j: [] },
    });
  }
}

function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers) {
  switch (bag.$) {
    case 1:
      var home = bag.k;
      var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
      effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
      return;

    case 2:
      var bags = bag.m;
      for (var idx = 0; idx < bags.length; idx++) {
        _Platform_gatherEffects(isCmd, bags[idx], effectsDict, taggers);
      }
      return;

    case 3:
      _Platform_gatherEffects(isCmd, bag.o, effectsDict, {
        s: bag.n,
        t: taggers,
      });
      return;
  }
}

function _Platform_toEffect(isCmd, home, taggers, value) {
  function applyTaggers(x) {
    for (var temp = taggers; temp; temp = temp.t) {
      x = temp.s(x);
    }
    return x;
  }

  var map = isCmd
    ? _Platform_effectManagers[home].e
    : _Platform_effectManagers[home].f;

  return A2(map, applyTaggers, value);
}

function _Platform_insert(isCmd, newEffect, effects) {
  effects = effects || { i: [], j: [] };

  isCmd
    ? (effects.i = A2($gren_lang$core$Array$pushLast, newEffect, effects.i))
    : (effects.j = A2($gren_lang$core$Array$pushLast, newEffect, effects.j));

  return effects;
}

// PORTS

function _Platform_checkPortName(name) {
  if (_Platform_effectManagers[name]) {
    _Debug_crash(3, name);
  }
}

// OUTGOING PORTS

function _Platform_outgoingPort(name, converter) {
  _Platform_checkPortName(name);
  _Platform_effectManagers[name] = {
    e: _Platform_outgoingPortMap,
    u: converter,
    a: _Platform_setupOutgoingPort,
  };
  return _Platform_leaf(name);
}

var _Platform_outgoingPortMap = F2(function (tagger, value) {
  return value;
});

function _Platform_setupOutgoingPort(name) {
  var subs = [];
  var converter = _Platform_effectManagers[name].u;

  // CREATE MANAGER

  var init = _Process_sleep(0);

  _Platform_effectManagers[name].b = init;
  _Platform_effectManagers[name].c = F3(function (
    router,
    cmdArray,
    state
  ) {
    for (var idx = 0; idx < cmdArray.length; idx++) {
      // grab a separate reference to subs in case unsubscribe is called
      var currentSubs = subs;
      var value = _Json_unwrap(converter(cmdArray[idx]));
      for (var subIdx = 0; subIdx < currentSubs.length; subIdx++) {
        currentSubs[subIdx](value);
      }
    }
    return init;
  });

  // PUBLIC API

  function subscribe(callback) {
    subs.push(callback);
  }

  function unsubscribe(callback) {
    // copy subs into a new array in case unsubscribe is called within a
    // subscribed callback
    subs = subs.slice();
    var index = subs.indexOf(callback);
    if (index >= 0) {
      subs.splice(index, 1);
    }
  }

  return {
    subscribe: subscribe,
    unsubscribe: unsubscribe,
  };
}

// INCOMING PORTS

function _Platform_incomingPort(name, converter) {
  _Platform_checkPortName(name);
  _Platform_effectManagers[name] = {
    f: _Platform_incomingPortMap,
    u: converter,
    a: _Platform_setupIncomingPort,
  };
  return _Platform_leaf(name);
}

var _Platform_incomingPortMap = F2(function (tagger, finalTagger) {
  return function (value) {
    return tagger(finalTagger(value));
  };
});

function _Platform_setupIncomingPort(name, sendToApp) {
  var subs = [];
  var converter = _Platform_effectManagers[name].u;

  // CREATE MANAGER

  var init = _Scheduler_succeed(null);

  _Platform_effectManagers[name].b = init;
  _Platform_effectManagers[name].c = F3(function (
    router,
    subArray,
    state
  ) {
    subs = subArray;
    return init;
  });

  // PUBLIC API

  function send(incomingValue) {
    var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

    $gren_lang$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

    var value = result.a;
    for (var idx = 0; idx < subs.length; idx++) {
      sendToApp(subs[idx](value));
    }
  }

  return { send: send };
}

// EXPORT GREN MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//

function _Platform_export_UNUSED(exports) {
  scope["Gren"]
    ? _Platform_mergeExportsProd(scope["Gren"], exports)
    : (scope["Gren"] = exports);
}

function _Platform_mergeExportsProd(obj, exports) {
  for (var name in exports) {
    name in obj
      ? name == "init"
        ? _Debug_crash(6)
        : _Platform_mergeExportsProd(obj[name], exports[name])
      : (obj[name] = exports[name]);
  }
}

function _Platform_export(exports) {
  scope["Gren"]
    ? _Platform_mergeExportsDebug("Gren", scope["Gren"], exports)
    : (scope["Gren"] = exports);
}

function _Platform_mergeExportsDebug(moduleName, obj, exports) {
  for (var name in exports) {
    name in obj
      ? name == "init"
        ? _Debug_crash(6, moduleName)
        : _Platform_mergeExportsDebug(
            moduleName + "." + name,
            obj[name],
            exports[name]
          )
      : (obj[name] = exports[name]);
  }
}


// LOG

var _Debug_log_UNUSED = F2(function (tag, value) {
  return value;
});

var _Debug_log = F2(function (tag, value) {
  console.log(tag + ": " + _Debug_toString(value));
  return value;
});

// TODOS

function _Debug_todo(moduleName, region) {
  return function (message) {
    _Debug_crash(8, moduleName, region, message);
  };
}

function _Debug_todoCase(moduleName, region, value) {
  return function (message) {
    _Debug_crash(9, moduleName, region, value, message);
  };
}

// TO STRING

function _Debug_toString_UNUSED(value) {
  return "<internals>";
}

function _Debug_toString(value) {
  return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value) {
  if (value == null) {
    return _Debug_internalColor(ansi, "<null>");
  }

  if (typeof value === "function") {
    return _Debug_internalColor(ansi, "<function>");
  }

  if (typeof value === "boolean") {
    return _Debug_ctorColor(ansi, value ? "True" : "False");
  }

  if (typeof value === "number") {
    return _Debug_numberColor(ansi, value + "");
  }

  if (value instanceof String) {
    return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
  }

  if (typeof value === "string") {
    return _Debug_stringColor(
      ansi,
      '"' + _Debug_addSlashes(value, false) + '"'
    );
  }

  if (Array.isArray(value)) {
    var output = "[";

    value.length > 0 && (output += _Debug_toAnsiString(ansi, value[0]));

    for (var idx = 1; idx < value.length; idx++) {
      output += ", " + _Debug_toAnsiString(ansi, value[idx]);
    }

    return output + "]";
  }

  if (typeof value === "object" && "$" in value) {
    var tag = value.$;

    if (typeof tag === "number") {
      return _Debug_internalColor(ansi, "<internals>");
    }

    if (tag === "Set_gren_builtin") {
      return (
        _Debug_ctorColor(ansi, "Set") +
        _Debug_fadeColor(ansi, ".fromArray") +
        " " +
        _Debug_toAnsiString(ansi, $gren_lang$core$Set$toArray(value))
      );
    }

    if (tag === "RBNode_gren_builtin" || tag === "RBEmpty_gren_builtin") {
      return (
        _Debug_ctorColor(ansi, "Dict") +
        _Debug_fadeColor(ansi, ".fromArray") +
        " " +
        _Debug_toAnsiString(ansi, A3($gren_lang$core$Dict$foldl, F3(function (key, value, acc) { acc.push({ key: key, value: value }); return acc; }), [], value))
      );
    }

    var output = "";
    for (var i in value) {
      if (i === "$") continue;
      var str = _Debug_toAnsiString(ansi, value[i]);
      var c0 = str[0];
      var parenless =
        c0 === "{" ||
        c0 === "(" ||
        c0 === "[" ||
        c0 === "<" ||
        c0 === '"' ||
        str.indexOf(" ") < 0;
      output += " " + (parenless ? str : "(" + str + ")");
    }
    return _Debug_ctorColor(ansi, tag) + output;
  }

  if (typeof DataView === "function" && value instanceof DataView) {
    return _Debug_stringColor(ansi, "<" + value.byteLength + " bytes>");
  }

  if (typeof File !== "undefined" && value instanceof File) {
    return _Debug_internalColor(ansi, "<" + value.name + ">");
  }

  if (typeof value === "object") {
    var output = [];
    for (var key in value) {
      var field = key[0] === "_" ? key.slice(1) : key;
      output.push(
        _Debug_fadeColor(ansi, field) +
          " = " +
          _Debug_toAnsiString(ansi, value[key])
      );
    }
    if (output.length === 0) {
      return "{}";
    }
    return "{ " + output.join(", ") + " }";
  }

  return _Debug_internalColor(ansi, "<internals>");
}

function _Debug_addSlashes(str, isChar) {
  var s = str
    .replace(/\\/g, "\\\\")
    .replace(/\n/g, "\\n")
    .replace(/\t/g, "\\t")
    .replace(/\r/g, "\\r")
    .replace(/\v/g, "\\v")
    .replace(/\0/g, "\\0");

  if (isChar) {
    return s.replace(/\'/g, "\\'");
  } else {
    return s.replace(/\"/g, '\\"');
  }
}

function _Debug_ctorColor(ansi, string) {
  return ansi ? "\x1b[96m" + string + "\x1b[0m" : string;
}

function _Debug_numberColor(ansi, string) {
  return ansi ? "\x1b[95m" + string + "\x1b[0m" : string;
}

function _Debug_stringColor(ansi, string) {
  return ansi ? "\x1b[93m" + string + "\x1b[0m" : string;
}

function _Debug_charColor(ansi, string) {
  return ansi ? "\x1b[92m" + string + "\x1b[0m" : string;
}

function _Debug_fadeColor(ansi, string) {
  return ansi ? "\x1b[37m" + string + "\x1b[0m" : string;
}

function _Debug_internalColor(ansi, string) {
  return ansi ? "\x1b[36m" + string + "\x1b[0m" : string;
}

function _Debug_toHexDigit(n) {
  return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}

// CRASH

function _Debug_crash_UNUSED(identifier) {
  throw new Error(
    "https://github.com/gren-lang/core/blob/1.0.0/hints/" + identifier + ".md"
  );
}

function _Debug_crash(identifier, fact1, fact2, fact3, fact4) {
  switch (identifier) {
    case 0:
      throw new Error(
        'What node should I take over? In JavaScript I need something like:\n\n    Gren.Main.init({\n        node: document.getElementById("gren-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.'
      );

    case 1:
      throw new Error(
        "Browser.application programs cannot handle URLs like this:\n\n    " +
          document.location.href +
          "\n\nWhat is the root? The root of your file system?"
      );

    case 2:
      var jsonErrorString = fact1;
      throw new Error(
        "Problem with the flags given to your Gren program on initialization.\n\n" +
          jsonErrorString
      );

    case 3:
      var portName = fact1;
      throw new Error(
        "There can only be one port named `" +
          portName +
          "`, but your program has multiple."
      );

    case 4:
      var portName = fact1;
      var problem = fact2;
      throw new Error(
        "Trying to send an unexpected type of value through port `" +
          portName +
          "`:\n" +
          problem
      );

    case 5:
      throw new Error(
        'Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Gren sense.\nRead more about this at https://package.gren-lang.org/packages/gren-lang/core/latest/Basics#== which describes why it is this way and what the better version will look like.'
      );

    case 6:
      var moduleName = fact1;
      throw new Error(
        "Your page is loading multiple Gren scripts with a module named " +
          moduleName +
          ". Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!"
      );

    case 8:
      var moduleName = fact1;
      var region = fact2;
      var message = fact3;
      throw new Error(
        "TODO in module `" +
          moduleName +
          "` " +
          _Debug_regionToString(region) +
          "\n\n" +
          message
      );

    case 9:
      var moduleName = fact1;
      var region = fact2;
      var value = fact3;
      var message = fact4;
      throw new Error(
        "TODO in module `" +
          moduleName +
          "` from the `case` expression " +
          _Debug_regionToString(region) +
          "\n\nIt received the following value:\n\n    " +
          _Debug_toString(value).replace("\n", "\n    ") +
          "\n\nBut the branch that handles it says:\n\n    " +
          message.replace("\n", "\n    ")
      );

    case 10:
      throw new Error("Bug in https://github.com/gren-lang/core/issues");

    case 11:
      throw new Error("Cannot perform mod 0. Division by zero error.");
  }
}

function _Debug_regionToString(region) {
  if (region.start.line === region.end.line) {
    return "on line " + region.start.line;
  }
  return (
    "on lines " + region.start.line + " through " + region.end.line
  );
}
var $gren_lang$core$Dict$foldl = F3(function(func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_gren_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
				$temp$acc = A3(func, key, value, A3($gren_lang$core$Dict$foldl, func, acc, left)),
				$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});


var _Array_length = function (array) {
  return array.length;
};

var _Array_initialize = F3(function (size, offset, func) {
  var result = new Array(size);

  for (var i = 0; i < size; i++) {
    result[i] = func(offset + i);
  }

  return result;
});

var _Array_get = F2(function (index, array) {
  var value = array.at(index);

  if (value === undefined) {
    return $gren_lang$core$Maybe$Nothing;
  }

  return $gren_lang$core$Maybe$Just(value);
});

var _Array_set = F3(function (index, value, array) {
  try {
    return array.with(index, value);
  } catch (e) {
    // assuming RangeError
    return array;
  }
});

var _Array_push = F2(function (value, array) {
  return array.concat(value);
});

var _Array_foldl = F3(function (func, acc, array) {
  for (var i = 0; i < array.length; i++) {
    acc = A2(func, array[i], acc);
  }

  return acc;
});

var _Array_foldr = F3(function (func, acc, array) {
  for (var i = array.length - 1; i >= 0; i--) {
    acc = A2(func, array[i], acc);
  }

  return acc;
});

var _Array_indexedFoldl = F3(function (func, acc, array) {
  for (var i = 0; i < array.length; i++) {
    acc = A3(func, i, array[i], acc);
  }

  return acc;
});

var _Array_indexedFoldr = F3(function (func, acc, array) {
  for (var i = array.length - 1; i >= 0; i--) {
    acc = A3(func, i, array[i], acc);
  }

  return acc;
});

var _Array_map = F2(function (func, array) {
  return array.map(func);
});

var _Array_indexedMap = F2(function (func, array) {
  return array.map(function (value, index) {
    return A2(func, index, value);
  });
});

var _Array_filter = F2(function (func, array) {
  return array.filter(func);
});

var _Array_flat = function (array) {
  return array.flat();
}

var _Array_flatMap = F2(function (func, array) {
  return array.flatMap(func);
});

var _Array_slice = F3(function (from, to, array) {
  return array.slice(from, to);
});

var _Array_append = F2(function (left, right) {
  return left.concat(right);
});

var _Array_reverse = function (array) {
  return array.toReversed();
};

var _Array_findFirst = F2(function (pred, array) {
  for (var i = 0; i < array.length; i++) {
    var element = array[i];

    if (pred(element)) {
      return $gren_lang$core$Maybe$Just(element);
    }
  }

  return $gren_lang$core$Maybe$Nothing;
});

var _Array_findLast = F2(function (pred, array) {
  for (var i = array.length - 1; i >= 0; i--) {
    var element = array[i];

    if (pred(element)) {
      return $gren_lang$core$Maybe$Just(element);
    }
  }

  return $gren_lang$core$Maybe$Nothing;
});

var _Array_map2 = F3(function (fn, as, bs) {
  var result = [];
  var lowestLength = as.length < bs.length ? as.length : bs.length;

  for (var i = 0; i < lowestLength; i++) {
    result.push(A2(fn, as[i], bs[i]));
  }

  return result;
});

var _Array_map3 = F4(function (fn, as, bs, cs) {
  var result = [];
  var lowestLength = [as.length, bs.length, cs.length].sort()[0];

  for (var i = 0; i < lowestLength; i++) {
    result.push(A3(fn, as[i], bs[i], cs[i]));
  }

  return result;
});

var _Array_sort = function (array) {
  return array.toSorted(function (a, b) {
    return _Utils_cmp(a, b);
  });
};

var _Array_sortBy = F2(function (fn, array) {
  return array.toSorted(function (a, b) {
    return _Utils_cmp(fn(a), fn(b));
  });
});

var _Array_sortWith = F2(function (fn, array) {
  return array.toSorted(function (a, b) {
    var ord = A2(fn, a, b);
    return ord === $gren_lang$core$Basics$EQ ? 0 : ord === $gren_lang$core$Basics$LT ? -1 : 1;
  });
});


// EQUALITY

function _Utils_eq(x, y) {
  for (
    var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
    isEqual && (pair = stack.pop());
    isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
  ) {}

  return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack) {
  if (x === y) {
    return true;
  }

  if (typeof x !== "object" || x === null || y === null) {
    typeof x === "function" && _Debug_crash(5);
    return false;
  }

  if (depth > 100) {
    stack.push({ a: x, b: y });
    return true;
  }

  /**/
	if (x.$ === 'Set_gren_builtin')
	{
		x = $gren_lang$core$Set$toArray(x);
		y = $gren_lang$core$Set$toArray(y);
	}
	if (x.$ === 'RBNode_gren_builtin' || x.$ === 'RBEmpty_gren_builtin')
	{
		x = A3($gren_lang$core$Dict$foldl, F3(function(key, value, acc) { acc.push({ a: key, b: value }); return acc; }), [], x);
		y = A3($gren_lang$core$Dict$foldl, F3(function(key, value, acc) { acc.push({ a: key, b: value }); return acc; }), [], y);
	}
	//*/

  /**_UNUSED/
	if (x.$ < 0)
	{
		x = A3($gren_lang$core$Dict$foldl, F3(function(key, value, acc) { acc.push({ a: key, b: value }); return acc; }), [], x);
		y = A3($gren_lang$core$Dict$foldl, F3(function(key, value, acc) { acc.push({ a: key, b: value }); return acc; }), [], y);
	}
	//*/

  if (x instanceof DataView) {
    var length = x.byteLength;

    if (y.byteLength !== length) {
      return false;
    }

    for (var i = 0; i < length; ++i) {
      if (x.getUint8(i) !== y.getUint8(i)) {
        return false;
      }
    }
  }

  if (Array.isArray(x) && x.length !== y.length) {
    return false;
  }

  var nextDepth = depth + 1;

  for (var key in x) {
    if (!_Utils_eqHelp(x[key], y[key], nextDepth, stack)) {
      return false;
    }
  }

  return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function (a, b) {
  return !_Utils_eq(a, b);
});

// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y) {
  if (typeof x !== "object") {
    return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
  }

  /**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

  // At this point, we can only be comparing arrays
  for (var idx = 0; idx < x.length; idx++) {
    var ord = _Utils_cmp(x[idx], y[idx]);
    if (ord !== 0) return ord;
  }

  return x.length - y.length;
}

var _Utils_lt = F2(function (a, b) {
  return _Utils_cmp(a, b) < 0;
});
var _Utils_le = F2(function (a, b) {
  return _Utils_cmp(a, b) < 1;
});
var _Utils_gt = F2(function (a, b) {
  return _Utils_cmp(a, b) > 0;
});
var _Utils_ge = F2(function (a, b) {
  return _Utils_cmp(a, b) >= 0;
});

var _Utils_compare = F2(function (x, y) {
  var n = _Utils_cmp(x, y);
  return n < 0 ? $gren_lang$core$Basics$LT : n ? $gren_lang$core$Basics$GT : $gren_lang$core$Basics$EQ;
});

// COMMON VALUES

function _Utils_chr_UNUSED(c) {
  return c;
}
function _Utils_chr(c) {
  return new String(c);
}

// RECORDS

function _Utils_update(oldRecord, updatedFields) {
  var newRecord = {};

  for (var key in oldRecord) {
    newRecord[key] = oldRecord[key];
  }

  for (var key in updatedFields) {
    newRecord[key] = updatedFields[key];
  }

  return newRecord;
}

// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys) {
  // append Strings
  if (typeof xs === "string") {
    return xs + ys;
  }

  return xs.concat(ys);
}
var $gren_lang$core$Basics$EQ = { $: 'EQ' };
var $gren_lang$core$Basics$GT = { $: 'GT' };
var $gren_lang$core$Basics$LT = { $: 'LT' };
var $gren_lang$core$Maybe$Just = function (a) {
	return { $: 'Just', a: a };
};
var $gren_lang$core$Maybe$Nothing = { $: 'Nothing' };
var $gren_lang$core$Array$pushLast = _Array_push;
var $gren_lang$core$Dict$keys = function(dict) {
	return A3($gren_lang$core$Dict$foldl, F3(function(key, value, keyArray) {
				return A2($gren_lang$core$Array$pushLast, key, keyArray);
			}), [  ], dict);
};
var $gren_lang$core$Set$toArray = function(_v0) {
	var dict = _v0.a;
	return $gren_lang$core$Dict$keys(dict);
};


/**/
function _Json_errorToString(error)
{
	return $gren_lang$core$Json$Decode$errorToString(error);
}
//*/

// CORE DECODERS

function _Json_succeed(msg) {
  return {
    $: 0,
    a: msg,
  };
}

function _Json_fail(msg) {
  return {
    $: 1,
    a: msg,
  };
}

function _Json_decodePrim(decoder) {
  return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function (value) {
  return typeof value !== "number"
    ? _Json_expecting("an INT", value)
    : Math.trunc(value) === value
    ? $gren_lang$core$Result$Ok(value)
    : isFinite(value) && !(value % 1)
    ? $gren_lang$core$Result$Ok(value)
    : _Json_expecting("an INT", value);
});

var _Json_decodeBool = _Json_decodePrim(function (value) {
  return typeof value === "boolean"
    ? $gren_lang$core$Result$Ok(value)
    : _Json_expecting("a BOOL", value);
});

var _Json_decodeFloat = _Json_decodePrim(function (value) {
  return typeof value === "number"
    ? $gren_lang$core$Result$Ok(value)
    : _Json_expecting("a FLOAT", value);
});

var _Json_decodeValue = _Json_decodePrim(function (value) {
  return $gren_lang$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function (value) {
  return typeof value === "string"
    ? $gren_lang$core$Result$Ok(value)
    : value instanceof String
    ? $gren_lang$core$Result$Ok(value + "")
    : _Json_expecting("a STRING", value);
});

function _Json_decodeArray(decoder) {
  return { $: 3, b: decoder };
}

function _Json_decodeNull(value) {
  return { $: 4, c: value };
}

var _Json_decodeField = F2(function (field, decoder) {
  return {
    $: 5,
    d: field,
    b: decoder,
  };
});

var _Json_decodeIndex = F2(function (index, decoder) {
  return {
    $: 6,
    e: index,
    b: decoder,
  };
});

function _Json_decodeKeyValuePairs(decoder) {
  return {
    $: 7,
    b: decoder,
  };
}

function _Json_mapMany(f, decoders) {
  return {
    $: 8,
    f: f,
    g: decoders,
  };
}

var _Json_andThen = F2(function (callback, decoder) {
  return {
    $: 9,
    b: decoder,
    h: callback,
  };
});

function _Json_oneOf(decoders) {
  return {
    $: 10,
    g: decoders,
  };
}

// DECODING OBJECTS

var _Json_map1 = F2(function (f, d1) {
  return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function (f, d1, d2) {
  return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function (f, d1, d2, d3) {
  return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function (f, d1, d2, d3, d4) {
  return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function (f, d1, d2, d3, d4, d5) {
  return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function (f, d1, d2, d3, d4, d5, d6) {
  return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function (f, d1, d2, d3, d4, d5, d6, d7) {
  return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function (f, d1, d2, d3, d4, d5, d6, d7, d8) {
  return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});

// DECODE

var _Json_runOnString = F2(function (decoder, string) {
  try {
    var value = JSON.parse(string);
    return _Json_runHelp(decoder, value);
  } catch (e) {
    return $gren_lang$core$Result$Err(
      A2(
        $gren_lang$core$Json$Decode$Failure,
        "This is not valid JSON! " + e.message,
        _Json_wrap(string)
      )
    );
  }
});

var _Json_run = F2(function (decoder, value) {
  return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value) {
  switch (decoder.$) {
    case 2:
      return decoder.b(value);

    case 4:
      return value === null
        ? $gren_lang$core$Result$Ok(decoder.c)
        : _Json_expecting("null", value);

    case 3:
      if (!_Json_isArray(value)) {
        return _Json_expecting("an ARRAY", value);
      }
      return _Json_runArrayDecoder(decoder.b, value);

    case 5:
      var field = decoder.d;
      if (typeof value !== "object" || value === null || !(field in value)) {
        return _Json_expecting(
          "an OBJECT with a field named `" + field + "`",
          value
        );
      }
      var result = _Json_runHelp(decoder.b, value[field]);
      return $gren_lang$core$Result$isOk(result)
        ? result
        : $gren_lang$core$Result$Err(A2($gren_lang$core$Json$Decode$Field, field, result.a));

    case 6:
      var index = decoder.e;
      if (!_Json_isArray(value)) {
        return _Json_expecting("an ARRAY", value);
      }
      if (index >= value.length) {
        return _Json_expecting(
          "a LONGER array. Need index " +
            index +
            " but only see " +
            value.length +
            " entries",
          value
        );
      }
      var result = _Json_runHelp(decoder.b, value[index]);
      return $gren_lang$core$Result$isOk(result)
        ? result
        : $gren_lang$core$Result$Err(A2($gren_lang$core$Json$Decode$Index, index, result.a));

    case 7:
      if (typeof value !== "object" || value === null || _Json_isArray(value)) {
        return _Json_expecting("an OBJECT", value);
      }

      var keyValuePairs = [];
      for (var key in value) {
        if (value.hasOwnProperty(key)) {
          var result = _Json_runHelp(decoder.b, value[key]);
          if (!$gren_lang$core$Result$isOk(result)) {
            return $gren_lang$core$Result$Err(A2($gren_lang$core$Json$Decode$Field, key, result.a));
          }
          keyValuePairs.push({ key: key, value: result.a });
        }
      }
      return $gren_lang$core$Result$Ok(keyValuePairs);

    case 8:
      var answer = decoder.f;
      var decoders = decoder.g;
      for (var i = 0; i < decoders.length; i++) {
        var result = _Json_runHelp(decoders[i], value);
        if (!$gren_lang$core$Result$isOk(result)) {
          return result;
        }
        answer = answer(result.a);
      }
      return $gren_lang$core$Result$Ok(answer);

    case 9:
      var result = _Json_runHelp(decoder.b, value);
      return !$gren_lang$core$Result$isOk(result)
        ? result
        : _Json_runHelp(decoder.h(result.a), value);

    case 10:
      var errors = [];

      var decoders = decoder.g;
      for (var idx = 0; idx < decoders.length; idx++) {
        var result = _Json_runHelp(decoders[idx], value);
        if ($gren_lang$core$Result$isOk(result)) {
          return result;
        }
        errors.push(result.a);
      }

      return $gren_lang$core$Result$Err($gren_lang$core$Json$Decode$OneOf(errors));

    case 1:
      return $gren_lang$core$Result$Err(A2($gren_lang$core$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

    case 0:
      return $gren_lang$core$Result$Ok(decoder.a);
  }
}

function _Json_runArrayDecoder(decoder, value) {
  var len = value.length;
  var array = new Array(len);
  for (var i = 0; i < len; i++) {
    var result = _Json_runHelp(decoder, value[i]);
    if (!$gren_lang$core$Result$isOk(result)) {
      return $gren_lang$core$Result$Err(A2($gren_lang$core$Json$Decode$Index, i, result.a));
    }
    array[i] = result.a;
  }
  return $gren_lang$core$Result$Ok(array);
}

function _Json_isArray(value) {
  return (
    Array.isArray(value) ||
    (typeof FileList !== "undefined" && value instanceof FileList)
  );
}

function _Json_expecting(type, value) {
  return $gren_lang$core$Result$Err(
    A2($gren_lang$core$Json$Decode$Failure, "Expecting " + type, _Json_wrap(value))
  );
}

// EQUALITY

function _Json_equality(x, y) {
  if (x === y) {
    return true;
  }

  if (x.$ !== y.$) {
    return false;
  }

  switch (x.$) {
    case 0:
    case 1:
      return x.a === y.a;

    case 2:
      return x.b === y.b;

    case 4:
      return x.c === y.c;

    case 3:
    case 7:
      return _Json_equality(x.b, y.b);

    case 5:
      return (
        x.d === y.d && _Json_equality(x.b, y.b)
      );

    case 6:
      return (
        x.e === y.e && _Json_equality(x.b, y.b)
      );

    case 8:
      return (
        x.f === y.f && _Json_arrayEquality(x.g, y.g)
      );

    case 9:
      return (
        x.h === y.h &&
        _Json_equality(x.b, y.b)
      );

    case 10:
      return _Json_arrayEquality(x.g, y.g);
  }
}

function _Json_arrayEquality(aDecoders, bDecoders) {
  var len = aDecoders.length;
  if (len !== bDecoders.length) {
    return false;
  }
  for (var i = 0; i < len; i++) {
    if (!_Json_equality(aDecoders[i], bDecoders[i])) {
      return false;
    }
  }
  return true;
}

// ENCODE

var _Json_encode = F2(function (indentLevel, value) {
  return JSON.stringify(_Json_unwrap(value), null, indentLevel) + "";
});

function _Json_wrap(value) {
  return { $: 0, a: value };
}
function _Json_unwrap(value) {
  return value.a;
}

function _Json_wrap_UNUSED(value) {
  return value;
}
function _Json_unwrap_UNUSED(value) {
  return value;
}

function _Json_emptyArray() {
  return [];
}
function _Json_emptyObject() {
  return {};
}

var _Json_addField = F3(function (key, value, object) {
  object[key] = _Json_unwrap(value);
  return object;
});

function _Json_addEntry(func) {
  return F2(function (entry, array) {
    array.push(_Json_unwrap(func(entry)));
    return array;
  });
}

var _Json_encodeNull = _Json_wrap(null);
var $gren_lang$core$Result$Err = function (a) {
	return { $: 'Err', a: a };
};
var $gren_lang$core$Json$Decode$Failure$ = function (a, b) {
	return { $: 'Failure', a: a, b: b };
};
var $gren_lang$core$Json$Decode$Failure = F2($gren_lang$core$Json$Decode$Failure$);
var $gren_lang$core$Json$Decode$Field$ = function (a, b) {
	return { $: 'Field', a: a, b: b };
};
var $gren_lang$core$Json$Decode$Field = F2($gren_lang$core$Json$Decode$Field$);
var $gren_lang$core$Json$Decode$Index$ = function (a, b) {
	return { $: 'Index', a: a, b: b };
};
var $gren_lang$core$Json$Decode$Index = F2($gren_lang$core$Json$Decode$Index$);
var $gren_lang$core$Result$Ok = function (a) {
	return { $: 'Ok', a: a };
};
var $gren_lang$core$Json$Decode$OneOf = function (a) {
	return { $: 'OneOf', a: a };
};
var $gren_lang$core$Basics$False = { $: 'False' };


// MATH

var _Basics_add = F2(function (a, b) {
  return a + b;
});
var _Basics_sub = F2(function (a, b) {
  return a - b;
});
var _Basics_mul = F2(function (a, b) {
  return a * b;
});
var _Basics_fdiv = F2(function (a, b) {
  return a / b;
});
var _Basics_idiv = F2(function (a, b) {
  return Math.trunc(a / b);
});
var _Basics_pow = F2(Math.pow);

// MORE MATH

function _Basics_toFloat(x) {
  return x;
}
function _Basics_isInfinite(n) {
  return n === Infinity || n === -Infinity;
}

var _Basics_isNaN = isNaN;

// BOOLEANS

function _Basics_not(bool) {
  return !bool;
}
var _Basics_and = F2(function (a, b) {
  return a && b;
});
var _Basics_or = F2(function (a, b) {
  return a || b;
});
var _Basics_xor = F2(function (a, b) {
  return a !== b;
});
var $gren_lang$core$Basics$add = _Basics_add;


var _String_cons = F2(function (chr, str) {
  return chr + str;
});

function _String_uncons(string) {
  if (string.length <= 0) {
    return $gren_lang$core$Maybe$Nothing;
  }

  return $gren_lang$core$Maybe$Just({ first: _Utils_chr(string[0]), rest: string.slice(1) });
}

var _String_append = F2(function (a, b) {
  return a + b;
});

function _String_length(str) {
  return str.length;
}

var _String_map = F2(function (func, string) {
  var len = string.length;
  var array = new Array(len);
  var i = 0;
  while (i < len) {
    array[i] = func(_Utils_chr(string[i]));
    i++;
  }
  return array.join("");
});

var _String_filter = F2(function (isGood, str) {
  var arr = [];
  var len = str.length;
  var i = 0;
  while (i < len) {
    var char = str[i];
    i++;

    if (isGood(_Utils_chr(char))) {
      arr.push(char);
    }
  }
  return arr.join("");
});

function _String_reverse(str) {
  var len = str.length;
  var arr = new Array(len);
  var i = 0;
  while (i < len) {
    arr[len - i] = str[i];
    i++;
  }
  return arr.join("");
}

var _String_foldl = F3(function (func, state, string) {
  var len = string.length;
  var i = 0;
  while (i < len) {
    var char = string[i];
    state = A2(func, _Utils_chr(char), state);
    i++;
  }
  return state;
});

var _String_foldr = F3(function (func, state, string) {
  var i = string.length;
  while (i--) {
    var char = string[i];
    state = A2(func, _Utils_chr(char), state);
  }
  return state;
});

var _String_split = F2(function (sep, str) {
  return str.split(sep);
});

var _String_join = F2(function (sep, strs) {
  return strs.join(sep);
});

var _String_slice = F3(function (start, end, str) {
  return str.slice(start, end);
});

function _String_trim(str) {
  return str.trim();
}

function _String_trimLeft(str) {
  return str.replace(/^\s+/, "");
}

function _String_trimRight(str) {
  return str.replace(/\s+$/, "");
}

function _String_words(str) {
  return str.trim().split(/\s+/g);
}

function _String_lines(str) {
  return str.split(/\r\n|\r|\n/g);
}

function _String_toUpper(str) {
  return str.toUpperCase();
}

function _String_toLower(str) {
  return str.toLowerCase();
}

var _String_any = F2(function (isGood, string) {
  var i = string.length;
  while (i--) {
    var char = string[i];
    if (isGood(_Utils_chr(char))) {
      return true;
    }
  }
  return false;
});

var _String_all = F2(function (isGood, string) {
  var i = string.length;
  while (i--) {
    var char = string[i];
    if (!isGood(_Utils_chr(char))) {
      return false;
    }
  }
  return true;
});

var _String_contains = F2(function (sub, str) {
  return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function (sub, str) {
  return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function (sub, str) {
  return (
    str.length >= sub.length && str.lastIndexOf(sub) === str.length - sub.length
  );
});

var _String_indexes = F2(function (sub, str) {
  var subLen = sub.length;

  if (subLen < 1) {
    return [];
  }

  var i = 0;
  var is = [];

  while ((i = str.indexOf(sub, i)) > -1) {
    is.push(i);
    i = i + subLen;
  }

  return is;
});

// TO STRING

function _String_fromNumber(number) {
  return number + "";
}

// INT CONVERSIONS

function _String_toInt(str) {
  var total = 0;
  var code0 = str.charCodeAt(0);
  var start = code0 == 0x2b /* + */ || code0 == 0x2d /* - */ ? 1 : 0;

  for (var i = start; i < str.length; ++i) {
    var code = str.charCodeAt(i);
    if (code < 0x30 || 0x39 < code) {
      return $gren_lang$core$Maybe$Nothing;
    }
    total = 10 * total + code - 0x30;
  }

  return i == start
    ? $gren_lang$core$Maybe$Nothing
    : $gren_lang$core$Maybe$Just(code0 == 0x2d ? -total : total);
}

// FLOAT CONVERSIONS

function _String_toFloat(s) {
  // check if it is a hex, octal, or binary number
  if (s.length === 0 || /[\sxbo]/.test(s)) {
    return $gren_lang$core$Maybe$Nothing;
  }
  var n = +s;
  // faster isNaN check
  return n === n ? $gren_lang$core$Maybe$Just(n) : $gren_lang$core$Maybe$Nothing;
}

function _String_fromArray(chars) {
  return chars.join("");
}
var $gren_lang$core$String$all = _String_all;
var $gren_lang$core$Basics$and = _Basics_and;
var $gren_lang$core$Basics$append = _Utils_append;
var $gren_lang$core$Json$Encode$encode = _Json_encode;
var $gren_lang$core$String$fromInt = _String_fromNumber;
var $gren_lang$core$String$join = _String_join;
var $gren_lang$core$String$split = _String_split;
var $gren_lang$core$Json$Decode$indent = function(str) {
	return A2($gren_lang$core$String$join, '\n    ', A2($gren_lang$core$String$split, '\n', str));
};
var $gren_lang$core$Array$indexedMap = _Array_indexedMap;
var $gren_lang$core$Basics$le = _Utils_le;


function _Char_toCode(char) {
  var code = char.charCodeAt(0);
  if (0xd800 <= code && code <= 0xdbff) {
    return (code - 0xd800) * 0x400 + char.charCodeAt(1) - 0xdc00 + 0x10000;
  }
  return code;
}

function _Char_fromCode(code) {
  return _Utils_chr(
    code < 0 || 0x10ffff < code
      ? "\uFFFD"
      : code <= 0xffff
      ? String.fromCharCode(code)
      : ((code -= 0x10000),
        String.fromCharCode(
          Math.floor(code / 0x400) + 0xd800,
          (code % 0x400) + 0xdc00
        ))
  );
}

function _Char_toUpper(char) {
  return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char) {
  return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char) {
  return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char) {
  return _Utils_chr(char.toLocaleLowerCase());
}
var $gren_lang$core$Char$toCode = _Char_toCode;
var $gren_lang$core$Char$isLower = function(_char) {
	var code = $gren_lang$core$Char$toCode(_char);
	return (_Utils_cmp(97, code) < 1) && (_Utils_cmp(code, 122) < 1);
};
var $gren_lang$core$Char$isUpper = function(_char) {
	var code = $gren_lang$core$Char$toCode(_char);
	return (_Utils_cmp(code, 90) < 1) && (_Utils_cmp(65, code) < 1);
};
var $gren_lang$core$Basics$or = _Basics_or;
var $gren_lang$core$Char$isAlpha = function(_char) {
	return $gren_lang$core$Char$isLower(_char) || $gren_lang$core$Char$isUpper(_char);
};
var $gren_lang$core$Char$isDigit = function(_char) {
	var code = $gren_lang$core$Char$toCode(_char);
	return (_Utils_cmp(code, 57) < 1) && (_Utils_cmp(48, code) < 1);
};
var $gren_lang$core$Char$isAlphaNum = function(_char) {
	return $gren_lang$core$Char$isLower(_char) || ($gren_lang$core$Char$isUpper(_char) || $gren_lang$core$Char$isDigit(_char));
};
var $gren_lang$core$Array$length = _Array_length;
var $gren_lang$core$String$uncons = _String_uncons;
var $gren_lang$core$Json$Decode$errorOneOf = F2(function(i, error) {
		return _Utils_ap('\n\n(', _Utils_ap($gren_lang$core$String$fromInt(i + 1), _Utils_ap(') ', $gren_lang$core$Json$Decode$indent($gren_lang$core$Json$Decode$errorToString(error)))));
	});
var $gren_lang$core$Json$Decode$errorToString = function(error) {
	return A2($gren_lang$core$Json$Decode$errorToStringHelp, error, [  ]);
};
var $gren_lang$core$Json$Decode$errorToStringHelp = F2(function(error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $gren_lang$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.first;
							var rest = _v2.rest;
							return $gren_lang$core$Char$isAlpha(_char) && A2($gren_lang$core$String$all, $gren_lang$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? _Utils_ap('.', f) : _Utils_ap('[\'', _Utils_ap(f, '\']'));
					var $temp$error = err,
					$temp$context = _Utils_ap([ fieldName ], context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = _Utils_ap('[', _Utils_ap($gren_lang$core$String$fromInt(i), ']'));
					var $temp$error = err,
					$temp$context = _Utils_ap([ indexName ], context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					switch (errors.length) {
						case 0:
							return _Utils_ap('Ran into a Json.Decode.oneOf with no possibilities', function () {
									if (context.length === 0) {
										return '!';
									} else {
										return _Utils_ap(' at json', A2($gren_lang$core$String$join, '', context));
									}
								}());
						case 1:
							var err = errors[0];
							var $temp$error = err,
							$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						default:
							var starter = function () {
								if (context.length === 0) {
									return 'Json.Decode.oneOf';
								} else {
									return _Utils_ap('The Json.Decode.oneOf at json', A2($gren_lang$core$String$join, '', context));
								}
							}();
							var introduction = _Utils_ap(starter, _Utils_ap(' failed in the following ', _Utils_ap($gren_lang$core$String$fromInt($gren_lang$core$Array$length(errors)), ' ways:')));
							return A2($gren_lang$core$String$join, '\n\n', _Utils_ap([ introduction ], A2($gren_lang$core$Array$indexedMap, $gren_lang$core$Json$Decode$errorOneOf, errors)));
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (context.length === 0) {
							return 'Problem with the given value:\n\n';
						} else {
							return _Utils_ap('Problem with the value at json', _Utils_ap(A2($gren_lang$core$String$join, '', context), ':\n\n    '));
						}
					}();
					return _Utils_ap(introduction, _Utils_ap($gren_lang$core$Json$Decode$indent(A2($gren_lang$core$Json$Encode$encode, 4, json)), _Utils_ap('\n\n', msg)));
			}
		}
	});
var $gren_lang$core$Basics$True = { $: 'True' };
var $gren_lang$core$Result$isOk = function(result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};


function _Process_sleep(time) {
  return _Scheduler_binding(function (callback) {
    var id = setTimeout(function () {
      callback(_Scheduler_succeed({}));
    }, time);

    return function () {
      clearTimeout(id);
    };
  });
}
var $gren_lang$core$Dict$RBEmpty_gren_builtin = { $: 'RBEmpty_gren_builtin' };
var $gren_lang$core$Dict$empty = $gren_lang$core$Dict$RBEmpty_gren_builtin;
var $gren_lang$core$Dict$Black = { $: 'Black' };
var $gren_lang$core$Dict$RBNode_gren_builtin$ = function (a, b, c, d, e) {
	return { $: 'RBNode_gren_builtin', a: a, b: b, c: c, d: d, e: e };
};
var $gren_lang$core$Dict$RBNode_gren_builtin = F5($gren_lang$core$Dict$RBNode_gren_builtin$);
var $gren_lang$core$Dict$Red = { $: 'Red' };
var $gren_lang$core$Dict$balance$ = function(color, key, value, left, right) {
	if ((right.$ === 'RBNode_gren_builtin') && (right.a.$ === 'Red')) {
		var _v1 = right.a;
		var rK = right.b;
		var rV = right.c;
		var rLeft = right.d;
		var rRight = right.e;
		if ((left.$ === 'RBNode_gren_builtin') && (left.a.$ === 'Red')) {
			var _v3 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return $gren_lang$core$Dict$RBNode_gren_builtin$($gren_lang$core$Dict$Red, key, value, $gren_lang$core$Dict$RBNode_gren_builtin$($gren_lang$core$Dict$Black, lK, lV, lLeft, lRight), $gren_lang$core$Dict$RBNode_gren_builtin$($gren_lang$core$Dict$Black, rK, rV, rLeft, rRight));
		} else {
			return $gren_lang$core$Dict$RBNode_gren_builtin$(color, rK, rV, $gren_lang$core$Dict$RBNode_gren_builtin$($gren_lang$core$Dict$Red, key, value, left, rLeft), rRight);
		}
	} else {
		if ((((left.$ === 'RBNode_gren_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_gren_builtin')) && (left.d.a.$ === 'Red')) {
			var _v5 = left.a;
			var lK = left.b;
			var lV = left.c;
			var _v6 = left.d;
			var _v7 = _v6.a;
			var llK = _v6.b;
			var llV = _v6.c;
			var llLeft = _v6.d;
			var llRight = _v6.e;
			var lRight = left.e;
			return $gren_lang$core$Dict$RBNode_gren_builtin$($gren_lang$core$Dict$Red, lK, lV, $gren_lang$core$Dict$RBNode_gren_builtin$($gren_lang$core$Dict$Black, llK, llV, llLeft, llRight), $gren_lang$core$Dict$RBNode_gren_builtin$($gren_lang$core$Dict$Black, key, value, lRight, right));
		} else {
			return $gren_lang$core$Dict$RBNode_gren_builtin$(color, key, value, left, right);
		}
	}
};
var $gren_lang$core$Dict$balance = F5($gren_lang$core$Dict$balance$);
var $gren_lang$core$Basics$compare = _Utils_compare;
var $gren_lang$core$Dict$setHelp = F3(function(key, value, dict) {
		if (dict.$ === 'RBEmpty_gren_builtin') {
			return $gren_lang$core$Dict$RBNode_gren_builtin$($gren_lang$core$Dict$Red, key, value, $gren_lang$core$Dict$RBEmpty_gren_builtin, $gren_lang$core$Dict$RBEmpty_gren_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($gren_lang$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return $gren_lang$core$Dict$balance$(nColor, nKey, nValue, A3($gren_lang$core$Dict$setHelp, key, value, nLeft), nRight);
				case 'EQ':
					return $gren_lang$core$Dict$RBNode_gren_builtin$(nColor, nKey, value, nLeft, nRight);
				default:
					return $gren_lang$core$Dict$balance$(nColor, nKey, nValue, nLeft, A3($gren_lang$core$Dict$setHelp, key, value, nRight));
			}
		}
	});
var $gren_lang$core$Dict$set$ = function(key, value, dict) {
	var _v0 = A3($gren_lang$core$Dict$setHelp, key, value, dict);
	if ((_v0.$ === 'RBNode_gren_builtin') && (_v0.a.$ === 'Red')) {
		var _v1 = _v0.a;
		var k = _v0.b;
		var v = _v0.c;
		var l = _v0.d;
		var r = _v0.e;
		return $gren_lang$core$Dict$RBNode_gren_builtin$($gren_lang$core$Dict$Black, k, v, l, r);
	} else {
		var x = _v0;
		return x;
	}
};
var $gren_lang$core$Dict$set = F3($gren_lang$core$Dict$set$);
var $gren_lang$node$Internal$Stream$Stream$ = function (a, b) {
	return { $: 'Stream', a: a, b: b };
};
var $gren_lang$node$Internal$Stream$Stream = F2($gren_lang$node$Internal$Stream$Stream$);
var $gren_lang$node$Node$Arm = { $: 'Arm' };
var $gren_lang$node$Node$Arm64 = { $: 'Arm64' };
var $gren_lang$node$Node$IA32 = { $: 'IA32' };
var $gren_lang$node$Node$Mips = { $: 'Mips' };
var $gren_lang$node$Node$Mipsel = { $: 'Mipsel' };
var $gren_lang$node$Node$PPC = { $: 'PPC' };
var $gren_lang$node$Node$PPC64 = { $: 'PPC64' };
var $gren_lang$node$Node$S390 = { $: 'S390' };
var $gren_lang$node$Node$S390x = { $: 'S390x' };
var $gren_lang$node$Node$UnknownArchitecture = function (a) {
	return { $: 'UnknownArchitecture', a: a };
};
var $gren_lang$node$Node$X64 = { $: 'X64' };
var $gren_lang$core$String$toLower = _String_toLower;
var $gren_lang$node$Node$archFromString = function(arch) {
	var _v0 = $gren_lang$core$String$toLower(arch);
	switch (_v0) {
		case 'arm':
			return $gren_lang$node$Node$Arm;
		case 'arm64':
			return $gren_lang$node$Node$Arm64;
		case 'ia32':
			return $gren_lang$node$Node$IA32;
		case 'mips':
			return $gren_lang$node$Node$Mips;
		case 'mipsel':
			return $gren_lang$node$Node$Mipsel;
		case 'ppc':
			return $gren_lang$node$Node$PPC;
		case 'ppc64':
			return $gren_lang$node$Node$PPC64;
		case 's390':
			return $gren_lang$node$Node$S390;
		case 's390x':
			return $gren_lang$node$Node$S390x;
		case 'x64':
			return $gren_lang$node$Node$X64;
		default:
			return $gren_lang$node$Node$UnknownArchitecture(arch);
	}
};
var $gren_lang$core$Task$succeed = _Scheduler_succeed;
var $gren_lang$core$Task$map$ = function(func, taskA) {
	return A2($gren_lang$core$Task$andThen, function(a) {
			return $gren_lang$core$Task$succeed(func(a));
		}, taskA);
};
var $gren_lang$core$Task$map = F2($gren_lang$core$Task$map$);
var $gren_lang$node$Node$Aix = { $: 'Aix' };
var $gren_lang$node$Node$Darwin = { $: 'Darwin' };
var $gren_lang$node$Node$FreeBSD = { $: 'FreeBSD' };
var $gren_lang$node$Node$Linux = { $: 'Linux' };
var $gren_lang$node$Node$OpenBSD = { $: 'OpenBSD' };
var $gren_lang$node$Node$SunOS = { $: 'SunOS' };
var $gren_lang$node$Node$UnknownPlatform = function (a) {
	return { $: 'UnknownPlatform', a: a };
};
var $gren_lang$node$Node$Win32 = { $: 'Win32' };
var $gren_lang$node$Node$platformFromString = function(platform) {
	var _v0 = $gren_lang$core$String$toLower(platform);
	switch (_v0) {
		case 'win32':
			return $gren_lang$node$Node$Win32;
		case 'darwin':
			return $gren_lang$node$Node$Darwin;
		case 'linux':
			return $gren_lang$node$Node$Linux;
		case 'freebsd':
			return $gren_lang$node$Node$FreeBSD;
		case 'openbsd':
			return $gren_lang$node$Node$OpenBSD;
		case 'sunos':
			return $gren_lang$node$Node$SunOS;
		case 'aix':
			return $gren_lang$node$Node$Aix;
		default:
			return $gren_lang$node$Node$UnknownPlatform(platform);
	}
};
var $gren_lang$node$Node$initializeEnvironment = $gren_lang$core$Task$map$(function(raw) {
		return { applicationPath: raw.applicationPath, args: raw.args, cpuArchitecture: $gren_lang$node$Node$archFromString(raw.arch), platform: $gren_lang$node$Node$platformFromString(raw.platform), stderr: $gren_lang$node$Internal$Stream$Stream$(1, raw.stderr), stdin: $gren_lang$node$Internal$Stream$Stream$(2, raw.stdin), stdout: $gren_lang$node$Internal$Stream$Stream$(0, raw.stdout) };
	}, _Node_init);
var $gren_lang$core$Task$Perform = function (a) {
	return { $: 'Perform', a: a };
};
var $gren_lang$core$Task$init = $gren_lang$core$Task$succeed({  });
var $gren_lang$core$Array$map = _Array_map;
var $gren_lang$core$Array$foldr = _Array_foldr;
var $gren_lang$core$Task$map2$ = function(func, taskA, taskB) {
	return A2($gren_lang$core$Task$andThen, function(a) {
			return A2($gren_lang$core$Task$andThen, function(b) {
					return $gren_lang$core$Task$succeed(A2(func, a, b));
				}, taskB);
		}, taskA);
};
var $gren_lang$core$Task$map2 = F3($gren_lang$core$Task$map2$);
var $gren_lang$core$Array$prepend = _Array_append;
var $gren_lang$core$Array$pushFirst$ = function(value, array) {
	return A2($gren_lang$core$Array$prepend, [ value ], array);
};
var $gren_lang$core$Array$pushFirst = F2($gren_lang$core$Array$pushFirst$);
var $gren_lang$core$Task$sequence = function(tasks) {
	return A3($gren_lang$core$Array$foldr, $gren_lang$core$Task$map2($gren_lang$core$Array$pushFirst), $gren_lang$core$Task$succeed([  ]), tasks);
};
var $gren_lang$core$Platform$sendToApp = _Platform_sendToApp;
var $gren_lang$core$Task$spawnCmd$ = function(router, cmd) {
	if (cmd.$ === 'Perform') {
		var task = cmd.a;
		return _Scheduler_spawn(A2($gren_lang$core$Task$andThen, $gren_lang$core$Platform$sendToApp(router), task));
	} else {
		var task = cmd.a;
		return _Scheduler_spawn(task);
	}
};
var $gren_lang$core$Task$spawnCmd = F2($gren_lang$core$Task$spawnCmd$);
var $gren_lang$core$Task$onEffects$ = function(router, commands, state) {
	return $gren_lang$core$Task$map$(function(_v0) {
			return {  };
		}, $gren_lang$core$Task$sequence(A2($gren_lang$core$Array$map, $gren_lang$core$Task$spawnCmd(router), commands)));
};
var $gren_lang$core$Task$onEffects = F3($gren_lang$core$Task$onEffects$);
var $gren_lang$core$Task$onSelfMsg$ = function(_v0, _v1, _v2) {
	return $gren_lang$core$Task$succeed({  });
};
var $gren_lang$core$Task$onSelfMsg = F3($gren_lang$core$Task$onSelfMsg$);
var $gren_lang$core$Task$Execute = function (a) {
	return { $: 'Execute', a: a };
};
var $gren_lang$core$Task$cmdMap$ = function(tagger, cmd) {
	if (cmd.$ === 'Perform') {
		var task = cmd.a;
		return $gren_lang$core$Task$Perform($gren_lang$core$Task$map$(tagger, task));
	} else {
		var task = cmd.a;
		return $gren_lang$core$Task$Execute(task);
	}
};
var $gren_lang$core$Task$cmdMap = F2($gren_lang$core$Task$cmdMap$);
_Platform_effectManagers['Task'] = _Platform_createManager($gren_lang$core$Task$init, $gren_lang$core$Task$onEffects, $gren_lang$core$Task$onSelfMsg, $gren_lang$core$Task$cmdMap);
var $gren_lang$core$Task$command = _Platform_leaf('Task');
var $gren_lang$core$Task$perform$ = function(toMessage, task) {
	return $gren_lang$core$Task$command($gren_lang$core$Task$Perform($gren_lang$core$Task$map$(toMessage, task)));
};
var $gren_lang$core$Task$perform = F2($gren_lang$core$Task$perform$);
var $gren_lang$node$Node$unwrap = function(_v0) {
	var task = _v0.a;
	return task;
};
var $gren_lang$node$Node$init$ = function(initTask, _v0) {
	return { command: $gren_lang$core$Task$perform$($gren_lang$node$Node$InitDone, A2($gren_lang$core$Task$andThen, function(env) {
				return $gren_lang$node$Node$unwrap(initTask(env));
			}, $gren_lang$node$Node$initializeEnvironment)), model: $gren_lang$node$Node$Uninitialized };
};
var $gren_lang$node$Node$init = F2($gren_lang$node$Node$init$);
var $gren_lang$node$Node$MsgReceived = function (a) {
	return { $: 'MsgReceived', a: a };
};
var $gren_lang$core$Platform$Sub$map = _Platform_map;
var $gren_lang$core$Platform$Sub$batch = _Platform_batch;
var $gren_lang$core$Platform$Sub$none = $gren_lang$core$Platform$Sub$batch([  ]);
var $gren_lang$node$Node$subscriptions$ = function(appSubs, model) {
	if (model.$ === 'Uninitialized') {
		return $gren_lang$core$Platform$Sub$none;
	} else {
		var appModel = model.a;
		return A2($gren_lang$core$Platform$Sub$map, $gren_lang$node$Node$MsgReceived, appSubs(appModel));
	}
};
var $gren_lang$node$Node$subscriptions = F2($gren_lang$node$Node$subscriptions$);
var $gren_lang$node$Node$Initialized = function (a) {
	return { $: 'Initialized', a: a };
};
var $gren_lang$core$Platform$Cmd$map = _Platform_map;
var $gren_lang$core$Platform$Cmd$batch = _Platform_batch;
var $gren_lang$core$Platform$Cmd$none = $gren_lang$core$Platform$Cmd$batch([  ]);
var $gren_lang$node$Node$update$ = function(appUpdate, msg, model) {
	if (model.$ === 'Uninitialized') {
		if (msg.$ === 'InitDone') {
			var initResult = msg.a;
			return { command: A2($gren_lang$core$Platform$Cmd$map, $gren_lang$node$Node$MsgReceived, initResult.command), model: $gren_lang$node$Node$Initialized(initResult.model) };
		} else {
			return { command: $gren_lang$core$Platform$Cmd$none, model: model };
		}
	} else {
		var appModel = model.a;
		if (msg.$ === 'InitDone') {
			return { command: $gren_lang$core$Platform$Cmd$none, model: model };
		} else {
			var appMsg = msg.a;
			var updateResult = A2(appUpdate, appMsg, appModel);
			return { command: A2($gren_lang$core$Platform$Cmd$map, $gren_lang$node$Node$MsgReceived, updateResult.command), model: $gren_lang$node$Node$Initialized(updateResult.model) };
		}
	}
};
var $gren_lang$node$Node$update = F3($gren_lang$node$Node$update$);
var $gren_lang$core$Platform$worker = _Platform_worker;
var $gren_lang$node$Node$defineProgram = function(config) {
	return $gren_lang$core$Platform$worker({ init: $gren_lang$node$Node$init(config.init), subscriptions: $gren_lang$node$Node$subscriptions(config.subscriptions), update: $gren_lang$node$Node$update(config.update) });
};
var $author$project$Main$ExistanceChecked = function (a) {
	return { $: 'ExistanceChecked', a: a };
};
var $gren_lang$core$Basics$composeL$ = function(g, f, x) {
	return g(f(x));
};
var $gren_lang$core$Basics$composeL = F3($gren_lang$core$Basics$composeL$);
var $gren_lang$core$Task$onError = _Scheduler_onError;
var $gren_lang$core$Task$attempt$ = function(resultToMessage, task) {
	return $gren_lang$core$Task$command($gren_lang$core$Task$Perform(A2($gren_lang$core$Task$onError, A2($gren_lang$core$Basics$composeL, A2($gren_lang$core$Basics$composeL, $gren_lang$core$Task$succeed, resultToMessage), $gren_lang$core$Result$Err), A2($gren_lang$core$Task$andThen, A2($gren_lang$core$Basics$composeL, A2($gren_lang$core$Basics$composeL, $gren_lang$core$Task$succeed, resultToMessage), $gren_lang$core$Result$Ok), task))));
};
var $gren_lang$core$Task$attempt = F2($gren_lang$core$Task$attempt$);
var $gren_lang$core$Basics$identity = function(x) {
	return x;
};
var $gren_lang$node$Internal$Init$Task = function (a) {
	return { $: 'Task', a: a };
};
var $gren_lang$node$Init$unwrap = function(_v0) {
	var task = _v0.a;
	return task;
};
var $gren_lang$node$Init$await$ = function(_v0, fn) {
	var task = _v0.a;
	return $gren_lang$node$Internal$Init$Task(A2($gren_lang$core$Task$andThen, A2($gren_lang$core$Basics$composeL, $gren_lang$node$Init$unwrap, fn), task));
};
var $gren_lang$node$Init$await = F2($gren_lang$node$Init$await$);
var $gren_lang$node$Init$awaitTask$ = function(task, fn) {
	return $gren_lang$node$Internal$Init$Task(A2($gren_lang$core$Task$andThen, A2($gren_lang$core$Basics$composeL, $gren_lang$node$Init$unwrap, fn), task));
};
var $gren_lang$node$Init$awaitTask = F2($gren_lang$node$Init$awaitTask$);


var fs = require("node:fs");
var bufferNs = require("node:buffer");
var process = require("node:process");
var path = require("node:path");
var os = require("node:os");

var _FileSystem_coerce = function (fh) {
  return fh;
};

var _FileSystem_open = F2(function (access, path) {
  return _Scheduler_binding(function (callback) {
    fs.open(_FilePath_toString(path), access, function (err, fd) {
      if (err != null) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(fd));
      }
    });
  });
});

var _FileSystem_constructError = function (err) {
  return A2($gren_lang$node$FileSystem$Error, err.code || "", err.message || "");
};

var _FileSystem_close = function (fh) {
  return _Scheduler_binding(function (callback) {
    fs.close(fh, function (err) {
      if (err != null) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed({}));
      }
    });
  });
};

var _FileSystem_readFromOffset = F2(function (fh, options) {
  var requestedLength =
    options.length < 0 || options.length > bufferNs.constants.MAX_LENGTH
      ? bufferNs.constants.MAX_LENGTH
      : options.length;

  var fileOffset = options.offset < 0 ? 0 : options.offset;

  return _Scheduler_binding(function (callback) {
    var initialBufferSize =
      requestedLength === bufferNs.constants.MAX_LENGTH
        ? 16 * 1024
        : requestedLength;
    var buffer = Buffer.allocUnsafe(initialBufferSize);

    _FileSystem_readHelper(
      fh,
      buffer,
      0,
      fileOffset,
      buffer.byteLength,
      requestedLength,
      callback
    );
  });
});

var _FileSystem_readHelper = function (
  fh,
  buffer,
  bufferOffset,
  fileOffset,
  maxReadLength,
  requestedReadLength,
  callback
) {
  fs.read(
    fh,
    buffer,
    bufferOffset,
    maxReadLength,
    fileOffset,
    function (err, bytesRead, _buff) {
      if (err != null) {
        callback(_Scheduler_fail(_FileSystem_constructError(err.message)));
        return;
      }

      var newBufferOffset = bufferOffset + bytesRead;

      if (bytesRead === 0 || newBufferOffset >= requestedReadLength) {
        callback(
          _Scheduler_succeed(
            new DataView(buffer.buffer, buffer.byteOffset, newBufferOffset)
          )
        );
        return;
      }

      var newMaxReadLength = maxReadLength - bytesRead;
      if (newMaxReadLength <= 0) {
        var oldBuffer = buffer;
        buffer = Buffer.allocUnsafe(oldBuffer.byteLength * 1.5);
        oldBuffer.copy(buffer);

        newMaxReadLength = buffer.byteLength - oldBuffer.byteLength;
      }

      _FileSystem_readHelper(
        fh,
        buffer,
        newBufferOffset,
        fileOffset + bytesRead,
        newMaxReadLength,
        requestedReadLength,
        callback
      );
    }
  );
};

var _FileSystem_writeFromOffset = F3(function (fh, options, bytes) {
  return _Scheduler_binding(function (callback) {
    _FileSystem_writeHelper(
      fh,
      bytes,
      0,
      bytes.byteLength,
      options.offset,
      callback
    );
  });
});

var _FileSystem_writeHelper = function (
  fh,
  buffer,
  bufferOffset,
  length,
  fileOffset,
  callback
) {
  fs.write(
    fh,
    buffer,
    bufferOffset,
    length,
    fileOffset,
    function (err, bytesWritten, buffer) {
      if (err != null) {
        callback(_Scheduler_fail(_FileSystem_constructError(err.message)));
        return;
      }

      if (bytesWritten === length) {
        callback(_Scheduler_succeed(fd));
        return;
      }

      var newBufferOffset = bufferOffset + bytesWritten;
      var newFileOffset = fileOffset + bytesWritten;

      _FileSystem_writeHelper(
        fh,
        buffer,
        newBufferOffset,
        length - bytesWritten,
        newFileOffset,
        callback
      );
    }
  );
};

var _FileSystem_remove = F2(function (options, path) {
  var rmOpts = {
    force: options.ignoreErrors,
    recursive: options.recursive,
  };

  return _Scheduler_binding(function (callback) {
    fs.rm(_FilePath_toString(path), rmOpts, function (err) {
      if (err != null) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(path));
      }
    });
  });
});

var _FileSystem_makeDirectory = F2(function (options, path) {
  return _Scheduler_binding(function (callback) {
    fs.mkdir(
      _FilePath_toString(path),
      { recursive: options.recursive },
      function (err) {
        if (err != null) {
          callback(_Scheduler_fail(_FileSystem_constructError(err)));
        } else {
          callback(_Scheduler_succeed(path));
        }
      }
    );
  });
});

// List of dir contents as DirEntry values holding filename string
var _FileSystem_listDirectory = function (path) {
  return _Scheduler_binding(function (callback) {
    fs.readdir(
      _FilePath_toString(path),
      { withFileTypes: true },
      function (err, content) {
        if (err != null) {
          callback(_Scheduler_fail(_FileSystem_constructError(err)));
        } else {
          callback(
            _Scheduler_succeed(
              content.map((f) => ({
                name: f.name,
                entityType: _FileSystem_toEntityType(f),
              }))
            )
          );
        }
      }
    );
  });
};

var _FileSystem_toEntityType = function (dirEnt) {
  if (dirEnt.isFile()) {
    return $gren_lang$node$FileSystem$File;
  } else if (dirEnt.isDirectory()) {
    return $gren_lang$node$FileSystem$Directory;
  } else if (dirEnt.isFIFO()) {
    return $gren_lang$node$FileSystem$Pipe;
  } else if (dirEnt.isSocket()) {
    return $gren_lang$node$FileSystem$Socket;
  } else if (dirEnt.isSymbolicLink()) {
    return $gren_lang$node$FileSystem$Symlink;
  } else {
    return $gren_lang$node$FileSystem$Device;
  }
};

var _FileSystem_fchmod = F2(function (mode, fd) {
  return _Scheduler_binding(function (callback) {
    fs.fchmod(fd, mode, function (err) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err.message)));
      } else {
        callback(_Scheduler_succeed(fd));
      }
    });
  });
});

var _FileSystem_fchown = F2(function (ids, fd) {
  return _Scheduler_binding(function (callback) {
    fs.fchown(fd, ids.userID, ids.groupID, function (err) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err.message)));
      } else {
        callback(_Scheduler_succeed(fd));
      }
    });
  });
});

var _FileSystem_fdatasync = function (fd) {
  return _Scheduler_binding(function (callback) {
    fs.fdatasync(fd, function (err) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err.message)));
      } else {
        callback(_Scheduler_succeed(fd));
      }
    });
  });
};

var _FileSystem_fsync = function (fd) {
  return _Scheduler_binding(function (callback) {
    fs.fsync(fd, function (err) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err.message)));
      } else {
        callback(_Scheduler_succeed(fd));
      }
    });
  });
};

var _FileSystem_fstat = function (fd) {
  return _Scheduler_binding(function (callback) {
    fs.fstat(fd, function (err, stats) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err.message)));
      } else {
        callback(_Scheduler_succeed(_FileSystem_statToGrenRecord(stats)));
      }
    });
  });
};

var _FileSystem_ftruncate = F2(function (len, fd) {
  return _Scheduler_binding(function (callback) {
    fs.ftruncate(fd, len, function (err) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err.message)));
      } else {
        callback(_Scheduler_succeed(fd));
      }
    });
  });
});

var _FileSystem_futimes = F3(function (atime, mtime, fd) {
  return _Scheduler_binding(function (callback) {
    fs.futimes(fd, atime, mtime, function (err) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err.message)));
      } else {
        callback(_Scheduler_succeed(fd));
      }
    });
  });
});

var _FileSystem_access = F2(function (permissions, path) {
  var mode = fs.constants.F_OK;

  if (permissions.includes($gren_lang$node$FileSystem$Read)) {
    mode = mode | fs.constants.R_OK;
  }

  if (permissions.includes($gren_lang$node$FileSystem$Write)) {
    mode = mode | fs.constants.W_OK;
  }

  if (permissions.includes($gren_lang$node$FileSystem$Execute)) {
    mode = mode | fs.constants.X_OK;
  }

  return _Scheduler_binding(function (callback) {
    fs.access(_FilePath_toString(path), mode, function (err) {
      if (err != null) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(path));
      }
    });
  });
});

var _FileSystem_appendFile = F2(function (data, path) {
  return _Scheduler_binding(function (callback) {
    fs.appendFile(_FilePath_toString(path), data, function (err) {
      if (err != null) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(path));
      }
    });
  });
});

var _FileSystem_chmod = F2(function (mode, path) {
  return _Scheduler_binding(function (callback) {
    fs.chmod(_FilePath_toString(path), mode, function (err) {
      if (err != null) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(path));
      }
    });
  });
});

var _FileSystem_chown = F2(function (ids, path) {
  return _Scheduler_binding(function (callback) {
    fs.chown(
      _FilePath_toString(path),
      ids.userID,
      ids.groupID,
      function (err) {
        if (err) {
          callback(_Scheduler_fail(_FileSystem_constructError(err)));
        } else {
          callback(_Scheduler_succeed(path));
        }
      }
    );
  });
});

var _FileSystem_lchown = F2(function (ids, path) {
  return _Scheduler_binding(function (callback) {
    fs.lchown(
      _FilePath_toString(path),
      ids.userID,
      ids.groupID,
      function (err) {
        if (err) {
          callback(_Scheduler_fail(_FileSystem_constructError(err)));
        } else {
          callback(_Scheduler_succeed(path));
        }
      }
    );
  });
});

var _FileSystem_copyFile = F2(function (src, dest) {
  return _Scheduler_binding(function (callback) {
    fs.copyFile(
      _FilePath_toString(src),
      _FilePath_toString(dest),
      0,
      function (err) {
        if (err) {
          callback(_Scheduler_fail(_FileSystem_constructError(err)));
        } else {
          callback(_Scheduler_succeed(dest));
        }
      }
    );
  });
});

var _FileSystem_link = F2(function (src, dest) {
  return _Scheduler_binding(function (callback) {
    fs.link(
      _FilePath_toString(src),
      _FilePath_toString(dest),
      function (err) {
        if (err) {
          callback(_Scheduler_fail(_FileSystem_constructError(err)));
        } else {
          callback(_Scheduler_succeed(dest));
        }
      }
    );
  });
});

var _FileSystem_symlink = F2(function (src, dest) {
  return _Scheduler_binding(function (callback) {
    fs.symlink(
      _FilePath_toString(src),
      _FilePath_toString(dest),
      function (err) {
        if (err) {
          callback(_Scheduler_fail(_FileSystem_constructError(err)));
        } else {
          callback(_Scheduler_succeed(dest));
        }
      }
    );
  });
});

var _FileSystem_unlink = function (src) {
  return _Scheduler_binding(function (callback) {
    fs.unlink(_FilePath_toString(src), function (err) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(src));
      }
    });
  });
};

var _FileSystem_mkdtemp = function (prefix) {
  return _Scheduler_binding(function (callback) {
    fs.mkdtemp(path.join(os.tmpdir(), prefix), function (err, dir) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(_FilePath_fromString(dir)));
      }
    });
  });
};

var _FileSystem_readFile = function (path) {
  return _Scheduler_binding(function (callback) {
    fs.readFile(_FilePath_toString(path), function (err, data) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(
          _Scheduler_succeed(
            new DataView(data.buffer, data.byteOffset, data.byteLength)
          )
        );
      }
    });
  });
};

var _FileSystem_readLink = function (path) {
  return _Scheduler_binding(function (callback) {
    fs.readlink(_FilePath_toString(path), function (err, linkedPath) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(_FilePath_fromString(linkedPath)));
      }
    });
  });
};

var _FileSystem_rename = F2(function (oldPath, newPath) {
  return _Scheduler_binding(function (callback) {
    fs.rename(
      _FilePath_toString(oldPath),
      _FilePath_toString(newPath),
      function (err) {
        if (err) {
          callback(_Scheduler_fail(_FileSystem_constructError(err)));
        } else {
          callback(_Scheduler_succeed(newPath));
        }
      }
    );
  });
});

var _FileSystem_realpath = function (path) {
  return _Scheduler_binding(function (callback) {
    fs.realpath(_FilePath_toString(path), function (err, resolvedPath) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(_FilePath_fromString(resolvedPath)));
      }
    });
  });
};

var _FileSystem_stat = function (path) {
  return _Scheduler_binding(function (callback) {
    fs.stat(_FilePath_toString(path), function (err, stats) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(_FileSystem_statToGrenRecord(stats)));
      }
    });
  });
};

var _FileSystem_lstat = function (path) {
  return _Scheduler_binding(function (callback) {
    fs.lstat(_FilePath_toString(path), function (err, stats) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(_FileSystem_statToGrenRecord(stats)));
      }
    });
  });
};

var _FileSystem_statToGrenRecord = function (stats) {
  return {
    entityType: _FileSystem_toEntityType(stats),
    blockSize: stats.blksize,
    blocks: stats.blocks,
    byteSize: stats.size,
    created: $gren_lang$core$Time$millisToPosix(Math.floor(stats.birthtimeMs)),
    deviceID: stats.dev,
    groupID: stats.gid,
    lastAccessed: $gren_lang$core$Time$millisToPosix(Math.floor(stats.atimeMs)),
    lastChanged: $gren_lang$core$Time$millisToPosix(Math.floor(stats.ctimeMs)),
    lastModified: $gren_lang$core$Time$millisToPosix(Math.floor(stats.mtimeMs)),
    userID: stats.uid,
  };
};

var _FileSystem_truncate = F2(function (len, path) {
  return _Scheduler_binding(function (callback) {
    fs.truncate(_FilePath_toString(path), len, function (err) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(path));
      }
    });
  });
});

var _FileSystem_utimes = F3(function (atime, mtime, path) {
  return _Scheduler_binding(function (callback) {
    fs.utimes(_FilePath_toString(path), atime, mtime, function (err) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(path));
      }
    });
  });
});

var _FileSystem_lutimes = F3(function (atime, mtime, path) {
  return _Scheduler_binding(function (callback) {
    fs.lutimes(_FilePath_toString(path), atime, mtime, function (err) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(path));
      }
    });
  });
});

var _FileSystem_writeFile = F2(function (data, path) {
  return _Scheduler_binding(function (callback) {
    fs.writeFile(_FilePath_toString(path), data, function (err) {
      if (err) {
        callback(_Scheduler_fail(_FileSystem_constructError(err)));
      } else {
        callback(_Scheduler_succeed(path));
      }
    });
  });
});

var _FileSystem_watch = F3(function (path, isRecursive, sendToSelf) {
  return _Scheduler_binding(function (_callback) {
    var watcher = null;

    try {
      watcher = fs.watch(
        path,
        { recursive: isRecursive },
        function (eventType, filename) {
          var maybePath = filename
            ? $gren_lang$core$Maybe$Just(_FilePath_fromString(filename))
            : $gren_lang$core$Maybe$Nothing;

          if (eventType === "rename") {
            _Scheduler_rawSpawn(sendToSelf($gren_lang$node$FileSystem$Moved(maybePath)));
          } else if (eventType === "change") {
            _Scheduler_rawSpawn(sendToSelf($gren_lang$node$FileSystem$Changed(maybePath)));
          }

          // other change types are ignored
        }
      );
    } catch (e) {
      // ignore errors
    }

    return function () {
      if (watcher) {
        watcher.close();
      }
    };
  });
});
var _FileSystem_homeDir = _Scheduler_binding(function (callback) {
  callback(_Scheduler_succeed(_FilePath_fromString(os.homedir())));
});

var _FileSystem_currentWorkingDirectory = _Scheduler_binding(function (
  callback
) {
  callback(_Scheduler_succeed(_FilePath_fromString(process.cwd())));
});

var _FileSystem_tmpDir = _Scheduler_binding(function (callback) {
  callback(_Scheduler_succeed(_FilePath_fromString(os.tmpdir())));
});

var _FileSystem_devNull = _Scheduler_binding(function (callback) {
  callback(_Scheduler_succeed(_FilePath_fromString(os.devNull)));
});
var $gren_lang$node$FileSystem$Changed = function (a) {
	return { $: 'Changed', a: a };
};
var $gren_lang$node$FileSystem$Device = { $: 'Device' };
var $gren_lang$node$FileSystem$Directory = { $: 'Directory' };
var $gren_lang$node$FileSystem$Error$ = function (a, b) {
	return { $: 'Error', a: a, b: b };
};
var $gren_lang$node$FileSystem$Error = F2($gren_lang$node$FileSystem$Error$);
var $gren_lang$node$FileSystem$Execute = { $: 'Execute' };
var $gren_lang$node$FileSystem$File = { $: 'File' };
var $gren_lang$node$FileSystem$Moved = function (a) {
	return { $: 'Moved', a: a };
};
var $gren_lang$node$FileSystem$Pipe = { $: 'Pipe' };
var $gren_lang$node$FileSystem$Read = { $: 'Read' };
var $gren_lang$node$FileSystem$Socket = { $: 'Socket' };
var $gren_lang$node$FileSystem$Symlink = { $: 'Symlink' };
var $gren_lang$node$FileSystem$Write = { $: 'Write' };
var $gren_lang$core$Time$Posix = function (a) {
	return { $: 'Posix', a: a };
};
var $gren_lang$core$Time$millisToPosix = $gren_lang$core$Time$Posix;
var $gren_lang$node$FileSystem$checkAccess$ = function(_v0, permissions, path) {
	return A2(_FileSystem_access, permissions, path);
};
var $gren_lang$node$FileSystem$checkAccess = F3($gren_lang$node$FileSystem$checkAccess$);
var $gren_lang$core$Basics$sub = _Basics_sub;
var $author$project$Main$countDown = F2(function(n, rec) {
		countDown:
		while (true) {
			if (_Utils_cmp(n, 0) < 1) {
				return 0;
			} else {
				var $temp$n = n - 1,
				$temp$rec = rec;
				n = $temp$n;
				rec = $temp$rec;
				continue countDown;
			}
		}
	});
var $gren_lang$core$Array$slice = _Array_slice;
var $gren_lang$core$Array$dropFirst$ = function(n, array) {
	return A3($gren_lang$core$Array$slice, n, $gren_lang$core$Array$length(array), array);
};
var $gren_lang$core$Array$dropFirst = F2($gren_lang$core$Array$dropFirst$);
var $gren_lang$node$FileSystem$Path$empty = { directory: [  ], extension: '', filename: '', root: '' };
var $gren_lang$core$Basics$eq = _Utils_equal;
var $gren_lang$core$Task$execute = function(task) {
	return $gren_lang$core$Task$command($gren_lang$core$Task$Execute($gren_lang$core$Task$map$(function(_v0) {
					return {  };
				}, task)));
};
var $gren_lang$node$FileSystem$Path$fromPosixString = _FilePath_fromPosix;
var $gren_lang$node$FileSystem$Path$fromWin32String = _FilePath_fromWin32;
var $gren_lang$core$Dict$get = F2(function(targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_gren_builtin') {
				return $gren_lang$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($gren_lang$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
						$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $gren_lang$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
						$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $gren_lang$node$Node$getEnvironmentVariables = _Node_getEnvironmentVariables;
var $gren_lang$node$FileSystem$homeDirectory = function(_v0) {
	return _FileSystem_homeDir;
};
var $gren_lang$node$ChildProcess$Permission = { $: 'Permission' };
var $gren_lang$node$ChildProcess$initialize = $gren_lang$node$Internal$Init$Task($gren_lang$core$Task$succeed($gren_lang$node$ChildProcess$Permission));
var $gren_lang$node$FileSystem$Permission = { $: 'Permission' };
var $gren_lang$node$FileSystem$initialize = $gren_lang$node$Internal$Init$Task($gren_lang$core$Task$succeed($gren_lang$node$FileSystem$Permission));
var $gren_lang$node$HttpClient$AnyPermission = { $: 'AnyPermission' };
var $gren_lang$node$HttpClient$initialize = $gren_lang$node$Internal$Init$Task($gren_lang$core$Task$succeed($gren_lang$node$HttpClient$AnyPermission));


var process = require("node:process");

var _Terminal_init = _Scheduler_binding(function (callback) {
  callback(
    _Scheduler_succeed({
      isTTY: process.stdout.isTTY,
      colorDepth: process.stdout.getColorDepth
        ? process.stdout.getColorDepth()
        : 0,
      columns: process.stdout.columns,
      rows: process.stdout.rows,
    })
  );
});

var _Terminal_attachListener = function (sendToApp) {
  return _Scheduler_binding(function (_callback) {
    var listener = function (data) {
      _Scheduler_rawSpawn(
        sendToApp({
          columns: process.stdout.columns,
          rows: process.stdout.rows,
        })
      );
    };

    process.stdout.on("resize", listener);

    return function () {
      process.stdout.off("resize", listener);
      process.stdout.pause();
    };
  });
};

var _Terminal_setStdInRawMode = function (toggle) {
  return _Scheduler_binding(function (callback) {
    process.stdin.setRawMode(toggle);
    callback(_Scheduler_succeed({}));
  });
};

var _Terminal_setProcessTitle = function (title) {
  return _Scheduler_binding(function (callback) {
    process.title = title;
    callback(_Scheduler_succeed({}));
  });
};
var $gren_lang$node$Terminal$Permission = { $: 'Permission' };
var $gren_lang$node$Terminal$initialize = $gren_lang$node$Internal$Init$Task($gren_lang$core$Task$map$(function(raw) {
			return raw.isTTY ? $gren_lang$core$Maybe$Just({ colorDepth: raw.colorDepth, columns: raw.columns, permission: $gren_lang$node$Terminal$Permission, rows: raw.rows }) : $gren_lang$core$Maybe$Nothing;
		}, _Terminal_init));
var $gren_lang$core$Array$append$ = function(fst, second) {
	return A2($gren_lang$core$Array$prepend, second, fst);
};
var $gren_lang$core$Array$append = F2($gren_lang$core$Array$append$);
var $gren_lang$core$String$isEmpty = function(string) {
	return _Utils_eq(string, '');
};
var $gren_lang$node$FileSystem$Path$filenameWithExtension = function(path) {
	return $gren_lang$core$String$isEmpty(path.extension) ? path.filename : _Utils_ap(path.filename, _Utils_ap('.', path.extension));
};
var $gren_lang$core$Array$filter = _Array_filter;
var $gren_lang$core$Basics$neq = _Utils_notEqual;
var $gren_lang$node$FileSystem$Path$prepend$ = function(left, right) {
	return _Utils_update(left, { directory: A2($gren_lang$core$Array$filter, function(dir) {
				return !_Utils_eq(dir, '');
			}, $gren_lang$core$Array$append$(right.directory, A2($gren_lang$core$Array$pushLast, $gren_lang$node$FileSystem$Path$filenameWithExtension(left), left.directory))), extension: right.extension, filename: right.filename });
};
var $gren_lang$node$FileSystem$Path$prepend = F2($gren_lang$node$FileSystem$Path$prepend$);
var $gren_lang$node$FileSystem$Path$append$ = function(left, right) {
	return $gren_lang$node$FileSystem$Path$prepend$(right, left);
};
var $gren_lang$node$FileSystem$Path$append = F2($gren_lang$node$FileSystem$Path$append$);
var $author$project$Main$compilerVersion = '0.4.4';
var $gren_lang$core$Maybe$map$ = function(f, maybe) {
	if (maybe.$ === 'Just') {
		var value = maybe.a;
		return $gren_lang$core$Maybe$Just(f(value));
	} else {
		return $gren_lang$core$Maybe$Nothing;
	}
};
var $gren_lang$core$Maybe$map = F2($gren_lang$core$Maybe$map$);
var $gren_lang$core$Maybe$withDefault$ = function(_default, maybe) {
	if (maybe.$ === 'Just') {
		var value = maybe.a;
		return value;
	} else {
		return _default;
	}
};
var $gren_lang$core$Maybe$withDefault = F2($gren_lang$core$Maybe$withDefault$);
var $author$project$Main$makeLocalPath$ = function(platform, homeDir, envVars) {
	var startPath = function () {
		switch (platform.$) {
			case 'Win32':
				return $gren_lang$core$Maybe$withDefault$($gren_lang$node$FileSystem$Path$prepend$(homeDir, $gren_lang$node$FileSystem$Path$fromPosixString('AppData/Local')), $gren_lang$core$Maybe$map$($gren_lang$node$FileSystem$Path$fromWin32String, A2($gren_lang$core$Dict$get, 'LOCALAPPDATA', envVars)));
			case 'Darwin':
				return $gren_lang$node$FileSystem$Path$prepend$(homeDir, $gren_lang$node$FileSystem$Path$fromPosixString('Library/Caches'));
			default:
				return $gren_lang$core$Maybe$withDefault$($gren_lang$node$FileSystem$Path$append$($gren_lang$node$FileSystem$Path$fromPosixString('.cache'), homeDir), $gren_lang$core$Maybe$map$($gren_lang$node$FileSystem$Path$fromPosixString, A2($gren_lang$core$Dict$get, 'XDG_CACHE_HOME', envVars)));
		}
	}();
	var filename = function () {
		if (platform.$ === 'Win32') {
			return 'gren.exe';
		} else {
			return 'gren';
		}
	}();
	var endPath = $gren_lang$node$FileSystem$Path$fromPosixString(A2($gren_lang$core$String$join, '/', [ 'gren', $author$project$Main$compilerVersion, 'bin', filename ]));
	return $gren_lang$node$FileSystem$Path$prepend$(startPath, endPath);
};
var $author$project$Main$makeLocalPath = F3($author$project$Main$makeLocalPath$);
var $author$project$Main$makeRemotePath = function(filename) {
	return A2($gren_lang$core$String$join, '/', [ 'https://github.com/gren-lang/compiler/releases/download', $author$project$Main$compilerVersion, filename ]);
};


// BYTES

var _Bytes_empty = new DataView(new ArrayBuffer(0));

function _Bytes_length(bytes) {
  return bytes.byteLength;
}

var _Bytes_getHostEndianness = F2(function (le, be) {
  return _Scheduler_binding(function (callback) {
    callback(
      _Scheduler_succeed(
        new Uint8Array(new Uint32Array([1]))[0] === 1 ? le : be
      )
    );
  });
});

function _Bytes_fromString(str) {
  var encoder = new TextEncoder();
  var uint8s = encoder.encode(str);
  return new DataView(uint8s.buffer);
}

function _Bytes_toString(bytes) {
  var decoder = new TextDecoder("utf-8", { fatal: true });

  try {
    return $gren_lang$core$Maybe$Just(decoder.decode(bytes));
  } catch (e) {
    return $gren_lang$core$Maybe$Nothing;
  }
}

function _Bytes_join(arrayOfBytes) {
  var requiredSize = 0;
  for (var i = 0; i < arrayOfBytes.length; i++) {
    requiredSize += arrayOfBytes[i].byteLength;
  }

  var offset = 0;
  var result = new Uint8Array(requiredSize);
  
  for (var i = 0; i < arrayOfBytes.length; i++) {
    var currentBytes = new UInt8Array(arrayOfBytes[i].buffer);
    var currentByteLength = arrayOfBytes[i].byteLength;
    
    for (var j = 0; j < currentByteLength; j++) {
      result[offset] = currentBytes[j];
      offset++;
    }
  }

  return new DataView(result.buffer);
}

// ENCODERS

function _Bytes_encode(encoder) {
  var mutableBytes = new DataView(new ArrayBuffer($gren_lang$core$Bytes$Encode$getLength(encoder)));
  A3($gren_lang$core$Bytes$Encode$write, encoder, mutableBytes, 0);
  return mutableBytes;
}

// SIGNED INTEGERS

var _Bytes_write_i8 = F3(function (mb, i, n) {
  mb.setInt8(i, n);
  return i + 1;
});
var _Bytes_write_i16 = F4(function (mb, i, n, isLE) {
  mb.setInt16(i, n, isLE);
  return i + 2;
});
var _Bytes_write_i32 = F4(function (mb, i, n, isLE) {
  mb.setInt32(i, n, isLE);
  return i + 4;
});

// UNSIGNED INTEGERS

var _Bytes_write_u8 = F3(function (mb, i, n) {
  mb.setUint8(i, n);
  return i + 1;
});
var _Bytes_write_u16 = F4(function (mb, i, n, isLE) {
  mb.setUint16(i, n, isLE);
  return i + 2;
});
var _Bytes_write_u32 = F4(function (mb, i, n, isLE) {
  mb.setUint32(i, n, isLE);
  return i + 4;
});

// FLOATS

var _Bytes_write_f32 = F4(function (mb, i, n, isLE) {
  mb.setFloat32(i, n, isLE);
  return i + 4;
});
var _Bytes_write_f64 = F4(function (mb, i, n, isLE) {
  mb.setFloat64(i, n, isLE);
  return i + 8;
});

// BYTES

var _Bytes_write_bytes = F3(function (mb, offset, bytes) {
  for (var i = 0, len = bytes.byteLength, limit = len - 4; i <= limit; i += 4) {
    mb.setUint32(offset + i, bytes.getUint32(i));
  }
  for (; i < len; i++) {
    mb.setUint8(offset + i, bytes.getUint8(i));
  }
  return offset + len;
});

// DECODER

var _Bytes_decode = F2(function (decoder, bytes) {
  try {
    return $gren_lang$core$Maybe$Just(A2(decoder, bytes, 0).value);
  } catch (e) {
    if (e instanceof RangeError) {
      return $gren_lang$core$Maybe$Nothing;
    } else {
      throw e;
    }
  }
});

var _Bytes_read_i8 = F2(function (bytes, offset) {
  return { offset: offset + 1, value: bytes.getInt8(offset) };
});
var _Bytes_read_i16 = F3(function (isLE, bytes, offset) {
  return { offset: offset + 2, value: bytes.getInt16(offset, isLE) };
});
var _Bytes_read_i32 = F3(function (isLE, bytes, offset) {
  return { offset: offset + 4, value: bytes.getInt32(offset, isLE) };
});
var _Bytes_read_u8 = F2(function (bytes, offset) {
  return { offset: offset + 1, value: bytes.getUint8(offset) };
});
var _Bytes_read_u16 = F3(function (isLE, bytes, offset) {
  return { offset: offset + 2, value: bytes.getUint16(offset, isLE) };
});
var _Bytes_read_u32 = F3(function (isLE, bytes, offset) {
  return { offset: offset + 4, value: bytes.getUint32(offset, isLE) };
});
var _Bytes_read_f32 = F3(function (isLE, bytes, offset) {
  return { offset: offset + 4, value: bytes.getFloat32(offset, isLE) };
});
var _Bytes_read_f64 = F3(function (isLE, bytes, offset) {
  return { offset: offset + 8, value: bytes.getFloat64(offset, isLE) };
});

var _Bytes_read_bytes = F3(function (len, bytes, offset) {
  return {
    offset: offset + len,
    value: new DataView(bytes.buffer, bytes.byteOffset + offset, len),
  };
});

var _Bytes_decodeFailure = F2(function () {
  throw 0;
});
var $gren_lang$core$Bytes$Encode$getLength = function(builder) {
	switch (builder.$) {
		case 'I8':
			return 1;
		case 'I16':
			return 2;
		case 'I32':
			return 4;
		case 'U8':
			return 1;
		case 'U16':
			return 2;
		case 'U32':
			return 4;
		case 'F32':
			return 4;
		case 'F64':
			return 8;
		case 'Seq':
			var w = builder.a;
			return w;
		case 'Utf8':
			var w = builder.a;
			return w;
		default:
			var bs = builder.a;
			return _Bytes_length(bs);
	}
};
var $gren_lang$core$Bytes$LE = { $: 'LE' };
var $gren_lang$core$Array$foldl = _Array_foldl;
var $gren_lang$core$Bytes$Encode$write = F3(function(builder, mb, offset) {
		switch (builder.$) {
			case 'I8':
				var n = builder.a;
				return A3(_Bytes_write_i8, mb, offset, n);
			case 'I16':
				var e = builder.a;
				var n = builder.b;
				return A4(_Bytes_write_i16, mb, offset, n, _Utils_eq(e, $gren_lang$core$Bytes$LE));
			case 'I32':
				var e = builder.a;
				var n = builder.b;
				return A4(_Bytes_write_i32, mb, offset, n, _Utils_eq(e, $gren_lang$core$Bytes$LE));
			case 'U8':
				var n = builder.a;
				return A3(_Bytes_write_u8, mb, offset, n);
			case 'U16':
				var e = builder.a;
				var n = builder.b;
				return A4(_Bytes_write_u16, mb, offset, n, _Utils_eq(e, $gren_lang$core$Bytes$LE));
			case 'U32':
				var e = builder.a;
				var n = builder.b;
				return A4(_Bytes_write_u32, mb, offset, n, _Utils_eq(e, $gren_lang$core$Bytes$LE));
			case 'F32':
				var e = builder.a;
				var n = builder.b;
				return A4(_Bytes_write_f32, mb, offset, n, _Utils_eq(e, $gren_lang$core$Bytes$LE));
			case 'F64':
				var e = builder.a;
				var n = builder.b;
				return A4(_Bytes_write_f64, mb, offset, n, _Utils_eq(e, $gren_lang$core$Bytes$LE));
			case 'Seq':
				var bs = builder.b;
				return A3($gren_lang$core$Bytes$Encode$writeSequence, bs, mb, offset);
			case 'Utf8':
				var s = builder.b;
				return A3(_Bytes_write_string, mb, offset, s);
			default:
				var bs = builder.a;
				return A3(_Bytes_write_bytes, mb, offset, bs);
		}
	});
var $gren_lang$core$Bytes$Encode$writeSequence = F3(function(builders, mb, offset) {
		return A3($gren_lang$core$Array$foldl, F2(function(builder, currentOffset) {
					return A3($gren_lang$core$Bytes$Encode$write, builder, mb, currentOffset);
				}), offset, builders);
	});
var $gren_lang$core$Bytes$fromString = _Bytes_fromString;


var _Stream_attachListener = F2(function (stream, sendToApp) {
  return _Scheduler_binding(function (_callback) {
    var listener = function (data) {
      _Scheduler_rawSpawn(
        sendToApp(new DataView(data.buffer, data.byteOffset, data.byteLength))
      );
    };

    stream.on("data", listener);

    return function () {
      stream.off("data", listener);
      stream.pause();
    };
  });
});

var _Stream_send = F2(function (stream, data) {
  return _Scheduler_binding(function (callback) {
    stream.write(new Uint8Array(data.buffer, data.byteOffset, data.byteLength));
    callback(_Scheduler_succeed({}));
  });
});
var $gren_lang$node$Stream$send$ = function(_v0, bytes) {
	var kernelStream = _v0.b;
	return A2(_Stream_send, kernelStream, bytes);
};
var $gren_lang$node$Stream$send = F2($gren_lang$node$Stream$send$);
var $gren_lang$node$Stream$sendString$ = function(stream, string) {
	return $gren_lang$node$Stream$send$(stream, $gren_lang$core$Bytes$fromString(string));
};
var $gren_lang$node$Stream$sendString = F2($gren_lang$node$Stream$sendString$);
var $gren_lang$node$Stream$sendLine$ = function(stream, string) {
	return $gren_lang$node$Stream$sendString$(stream, _Utils_ap(string, '\n'));
};
var $gren_lang$node$Stream$sendLine = F2($gren_lang$node$Stream$sendLine$);
var $gren_lang$node$Node$startProgram = function(initResult) {
	return $gren_lang$node$Internal$Init$Task($gren_lang$core$Task$succeed(initResult));
};
var $gren_lang$node$FileSystem$Path$toPosixString = _FilePath_toPosix;
var $gren_lang$node$FileSystem$Path$toWin32String = _FilePath_toWin32;
var $author$project$Main$init = function(env) {
	return $gren_lang$node$Init$await$($gren_lang$node$FileSystem$initialize, function(fsPermission) {
			return $gren_lang$node$Init$await$($gren_lang$node$ChildProcess$initialize, function(cpPermission) {
					return $gren_lang$node$Init$await$($gren_lang$node$HttpClient$initialize, function(httpPermission) {
							return $gren_lang$node$Init$await$($gren_lang$node$Terminal$initialize, function(terminalConfig) {
									return $gren_lang$node$Init$awaitTask$($gren_lang$node$Node$getEnvironmentVariables, function(envVars) {
											return $gren_lang$node$Init$awaitTask$($gren_lang$node$FileSystem$homeDirectory(fsPermission), function(homeDir) {
													var userArgs = $gren_lang$core$Array$dropFirst$(2, env.args);
													var useless = A2($author$project$Main$countDown, 10, {  });
													var useColor = function () {
														if (terminalConfig.$ === 'Nothing') {
															return false;
														} else {
															var _v10 = A2($gren_lang$core$Dict$get, 'NO_COLOR', envVars);
															if (_v10.$ === 'Just') {
																return false;
															} else {
																return true;
															}
														}
													}();
													var maybePaths = function () {
														var _v2 = { arch: env.cpuArchitecture, override: A2($gren_lang$core$Dict$get, 'GREN_BIN', envVars), platform: env.platform };
														_v2$1:
														while (true) {
															_v2$5:
															while (true) {
																switch (_v2.platform.$) {
																	case 'Win32':
																		if (_v2.override.$ === 'Just') {
																			var overridePath = _v2.override.a;
																			var _v3 = _v2.platform;
																			return $gren_lang$core$Maybe$Just({ args: userArgs, localPath: $gren_lang$node$FileSystem$Path$fromWin32String(overridePath), remotePath: $gren_lang$core$Maybe$Nothing, stdout: env.stdout });
																		} else {
																			if (_v2.arch.$ === 'X64') {
																				var _v4 = _v2.platform;
																				var _v5 = _v2.arch;
																				return $gren_lang$core$Maybe$Just({ args: userArgs, localPath: $author$project$Main$makeLocalPath$(env.platform, homeDir, envVars), remotePath: $gren_lang$core$Maybe$Just($author$project$Main$makeRemotePath('gren.exe')), stdout: env.stdout });
																			} else {
																				break _v2$5;
																			}
																		}
																	case 'Darwin':
																		if (_v2.override.$ === 'Just') {
																			break _v2$1;
																		} else {
																			var _v6 = _v2.platform;
																			return $gren_lang$core$Maybe$Just({ args: userArgs, localPath: $author$project$Main$makeLocalPath$(env.platform, homeDir, envVars), remotePath: $gren_lang$core$Maybe$Just($author$project$Main$makeRemotePath('gren_mac')), stdout: env.stdout });
																		}
																	case 'Linux':
																		if (_v2.override.$ === 'Just') {
																			break _v2$1;
																		} else {
																			if (_v2.arch.$ === 'X64') {
																				var _v7 = _v2.platform;
																				var _v8 = _v2.arch;
																				return $gren_lang$core$Maybe$Just({ args: userArgs, localPath: $author$project$Main$makeLocalPath$(env.platform, homeDir, envVars), remotePath: $gren_lang$core$Maybe$Just($author$project$Main$makeRemotePath('gren_linux')), stdout: env.stdout });
																			} else {
																				break _v2$5;
																			}
																		}
																	default:
																		if (_v2.override.$ === 'Just') {
																			break _v2$1;
																		} else {
																			break _v2$5;
																		}
																}
															}
															return $gren_lang$core$Maybe$Nothing;
														}
														var overridePath = _v2.override.a;
														return $gren_lang$core$Maybe$Just({ args: userArgs, localPath: $gren_lang$node$FileSystem$Path$fromPosixString(overridePath), remotePath: $gren_lang$core$Maybe$Nothing, stdout: env.stdout });
													}();
													var model = function () {
														if (maybePaths.$ === 'Just') {
															var paths = maybePaths.a;
															return { args: userArgs, cpPermission: cpPermission, fsPermission: fsPermission, httpPermission: httpPermission, localPath: paths.localPath, pathToString: _Utils_eq(env.platform, $gren_lang$node$Node$Win32) ? $gren_lang$node$FileSystem$Path$toWin32String : $gren_lang$node$FileSystem$Path$toPosixString, remotePath: paths.remotePath, stderr: env.stderr, stdout: env.stdout, useColor: useColor };
														} else {
															return { args: [  ], cpPermission: cpPermission, fsPermission: fsPermission, httpPermission: httpPermission, localPath: $gren_lang$node$FileSystem$Path$empty, pathToString: $gren_lang$node$FileSystem$Path$toPosixString, remotePath: $gren_lang$core$Maybe$Nothing, stderr: env.stderr, stdout: env.stdout, useColor: useColor };
														}
													}();
													return $gren_lang$node$Node$startProgram({ command: function () {
															if (maybePaths.$ === 'Just') {
																return $gren_lang$core$Task$attempt$($author$project$Main$ExistanceChecked, $gren_lang$node$FileSystem$checkAccess$(fsPermission, [  ], model.localPath));
															} else {
																return $gren_lang$core$Task$execute($gren_lang$node$Stream$sendLine$(env.stderr, 'We currently don\'t support this platform/arch.'));
															}
														}(), model: model });
												});
										});
								});
						});
				});
		});
};
var $gren_lang$core$Json$Decode$succeed = _Json_succeed;
var $author$project$Main$CompilerDownloaded = function (a) {
	return { $: 'CompilerDownloaded', a: a };
};
var $author$project$Main$CompilerInstalled = function (a) {
	return { $: 'CompilerInstalled', a: a };
};
var $gren_lang$core$Array$findFirst = _Array_findFirst;
var $gren_lang$core$Array$member$ = function(value, array) {
	var _v0 = A2($gren_lang$core$Array$findFirst, function(v) {
			return _Utils_eq(v, value);
		}, array);
	if (_v0.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $gren_lang$core$Array$member = F2($gren_lang$core$Array$member$);
var $gren_lang$node$FileSystem$accessPermissionsToInt = function(values) {
	var numberFor = F2(function(num, a) {
			return $gren_lang$core$Array$member$(a, values) ? num : 0;
		});
	return (A2(numberFor, 4, $gren_lang$node$FileSystem$Read) + A2(numberFor, 2, $gren_lang$node$FileSystem$Write)) + A2(numberFor, 1, $gren_lang$node$FileSystem$Execute);
};
var $gren_lang$node$FileSystem$changeAccess$ = function(_v0, permissions, path) {
	var mode = _Utils_ap($gren_lang$core$String$fromInt($gren_lang$node$FileSystem$accessPermissionsToInt(permissions.owner)), _Utils_ap($gren_lang$core$String$fromInt($gren_lang$node$FileSystem$accessPermissionsToInt(permissions.group)), $gren_lang$core$String$fromInt($gren_lang$node$FileSystem$accessPermissionsToInt(permissions.others))));
	return A2(_FileSystem_chmod, mode, path);
};
var $gren_lang$node$FileSystem$changeAccess = F3($gren_lang$node$FileSystem$changeAccess$);
var $gren_lang$node$HttpClient$ExpectBytes = { $: 'ExpectBytes' };
var $gren_lang$node$HttpClient$expectBytes = function(req) {
	return { body: req.body, expect: $gren_lang$node$HttpClient$ExpectBytes, headers: req.headers, method: req.method, timeout: req.timeout, url: req.url };
};
var $gren_lang$node$HttpServer$GET = { $: 'GET' };
var $gren_lang$node$HttpClient$BodyEmpty = { $: 'BodyEmpty' };
var $gren_lang$node$HttpClient$ExpectAnything = { $: 'ExpectAnything' };
var $gren_lang$core$Basics$mul = _Basics_mul;
var $gren_lang$node$HttpClient$defaultTimeout = 10 * 1000;
var $gren_lang$node$HttpClient$request$ = function(method, url) {
	return { body: $gren_lang$node$HttpClient$BodyEmpty, expect: $gren_lang$node$HttpClient$ExpectAnything, headers: $gren_lang$core$Dict$empty, method: method, timeout: $gren_lang$node$HttpClient$defaultTimeout, url: url };
};
var $gren_lang$node$HttpClient$request = F2($gren_lang$node$HttpClient$request$);
var $gren_lang$node$HttpClient$get = function(url) {
	return $gren_lang$node$HttpClient$request$($gren_lang$node$HttpServer$GET, url);
};


const http = require("node:http");
const https = require("node:https");
var buffer = require("node:buffer").Buffer;

function _HttpClient_clientForProtocol(config) {
  if (config.url.startsWith("http://")) {
    return http;
  }

  return https;
}

var _HttpClient_request = function (config) {
  return _Scheduler_binding(function (callback) {
    let req = null;
    try {
      const client = _HttpClient_clientForProtocol(config);
      req = client.request(config.url, {
        method: config.method,
        headers: A3(
          $gren_lang$core$Dict$foldl,
          _HttpClient_dictToObject,
          {},
          config.headers
        ),
        timeout: config.timeout,
      });
    } catch (e) {
      if (e.code === "ERR_INVALID_HTTP_TOKEN") {
        return callback(_Scheduler_fail($gren_lang$node$HttpClient$BadHeaders));
      } else if (e.code === "ERR_INVALID_URL") {
        return callback(_Scheduler_fail($gren_lang$node$HttpClient$BadUrl(config.url)));
      } else {
        return callback(
          _Scheduler_fail(
            $gren_lang$node$HttpClient$UnknownError("problem with request: " + e.message)
          )
        );
      }
    }

    req.on("timeout", () => {
      req.destroy(_HttpClient_CustomTimeoutError);
    });

    req.on("error", (e) => {
      if (e === _HttpClient_CustomTimeoutError) {
        return callback(_Scheduler_fail($gren_lang$node$HttpClient$Timeout));
      }

      return callback(
        _Scheduler_fail(
          $gren_lang$node$HttpClient$UnknownError("problem with request: " + e.message)
        )
      );
    });

    req.on("response", (res) => {
      const expectType = config.expectType;

      let rawData = null;

      if (expectType === "STRING" || expectType === "JSON") {
        res.setEncoding("utf8");

        res.on("data", (chunk) => {
          if (rawData === null) {
            rawData = chunk;
          } else {
            rawData += chunk;
          }
        });
      } else {
        res.on("data", (chunk) => {
          if (rawData === null) {
            rawData = [chunk];
          } else {
            rawData.push(chunk);
          }
        });
      }

      res.on("error", (e) => {
        return callback(
          _Scheduler_fail(
            $gren_lang$node$HttpClient$UnknownError("problem with request: " + e.message)
          )
        );
      });

      res.on("end", () => {
        if (res.statusCode < 200 || res.statusCode >= 300) {
          return callback(
            _Scheduler_fail(
              $gren_lang$node$HttpClient$BadStatus(_HttpClient_formatResponse(res, rawData))
            )
          );
        }

        switch (expectType) {
          case "NOTHING":
            if (rawData === null) {
              return callback(
                _Scheduler_succeed(_HttpClient_formatResponse(res, {}))
              );
            } else {
              return callback(
                _Scheduler_fail(
                  $gren_lang$node$HttpClient$UnexpectedResponseBody(
                    "Received response body where I expected none."
                  )
                )
              );
            }

          case "ANYTHING":
            return callback(
              _Scheduler_succeed(_HttpClient_formatResponse(res, {}))
            );

          case "STRING":
            return callback(
              _Scheduler_succeed(_HttpClient_formatResponse(res, rawData))
            );

          case "JSON":
            const jsonResult = A2(
              $gren_lang$core$Json$Decode$decodeString,
              config.expect.a,
              rawData
            );
            if ($gren_lang$core$Result$isOk(jsonResult)) {
              return callback(
                _Scheduler_succeed(
                  _HttpClient_formatResponse(res, jsonResult.a)
                )
              );
            } else {
              return callback(
                _Scheduler_fail(
                  $gren_lang$node$HttpClient$UnexpectedResponseBody(
                    $gren_lang$core$Json$Decode$errorToString(jsonResult.a)
                  )
                )
              );
            }

          case "BYTES":
            const finalBuffer = buffer.concat(rawData);

            return callback(
              _Scheduler_succeed(
                _HttpClient_formatResponse(
                  res,
                  new DataView(
                    finalBuffer.buffer,
                    finalBuffer.byteOffset,
                    finalBuffer.byteLength
                  )
                )
              )
            );
        }
      });
    });

    const body = _HttpClient_extractRequestBody(config);

    if (body != null) {
      req.end(body);
    } else {
      req.end();
    }
  });
};

var _HttpClient_stream = F4(function (cleanup, sendToApp, request, config) {
  return _Scheduler_binding(function (callback) {
    function send(msg) {
      return _Scheduler_rawSpawn(sendToApp(msg));
    }

    let req = null;
    try {
      const client = _HttpClient_clientForProtocol(config);
      req = client.request(config.url, {
        method: config.method,
        headers: A3(
          $gren_lang$core$Dict$foldl,
          _HttpClient_dictToObject,
          {},
          config.headers
        ),
        timeout: config.timeout,
      });
    } catch (e) {
      callback(_Scheduler_succeed(request));

      if (e.code === "ERR_INVALID_HTTP_TOKEN") {
        send($gren_lang$node$HttpClient$Error($gren_lang$node$HttpClient$BadHeaders));
      } else if (e.code === "ERR_INVALID_URL") {
        send($gren_lang$node$HttpClient$Error($gren_lang$node$HttpClient$BadUrl(config.url)));
      } else {
        send(
          $gren_lang$node$HttpClient$Error(
            $gren_lang$node$HttpClient$UnknownError("problem with request: " + e.message)
          )
        );
      }

      return _Scheduler_rawSpawn(cleanup(request));
    }

    req.on("timeout", () => {
      req.destroy(_HttpClient_CustomTimeoutError);
    });

    req.on("error", (e) => {
      _Scheduler_rawSpawn(cleanup(request));

      if (e === _HttpClient_CustomTimeoutError) {
        send($gren_lang$node$HttpClient$Timeout);
      } else if (e === _HttpClient_CustomAbortError) {
        send($gren_lang$node$HttpClient$Aborted);
      } else {
        send($gren_lang$node$HttpClient$UnknownError("problem with request: " + e.message));
      }
    });

    const body = _HttpClient_extractRequestBody(config);

    if (body == null) {
      send($gren_lang$node$HttpClient$SentChunk(request));
    } else {
      req.write(body, () => {
        send($gren_lang$node$HttpClient$SentChunk(request));
      });
    }

    return callback(
      _Scheduler_succeed({
        request: req,
        response: null,
      })
    );
  });
});

var _HttpClient_sendChunk = F4(function (
  sendToApp,
  kernelRequest,
  request,
  bytes
) {
  return _Scheduler_binding(function (callback) {
    if (!kernelRequest.request.writableEnded) {
      const chunk = _HttpClient_prepBytes(bytes);

      kernelRequest.request.write(chunk, () => {
        _Scheduler_rawSpawn(sendToApp($gren_lang$node$HttpClient$SentChunk(request)));
      });
    }

    return callback(_Scheduler_succeed({}));
  });
});

var _HttpClient_startReceive = F4(function (
  cleanup,
  sendToApp,
  kernelRequest,
  request
) {
  return _Scheduler_binding(function (callback) {
    if (kernelRequest.request.writableEnded) {
      return callback(_Scheduler_succeed({}));
    }
    kernelRequest.request.on("response", (res) => {
      kernelRequest.response = res;

      res.on("data", (bytes) => {
        return _Scheduler_rawSpawn(
          sendToApp(
            A2(
              $gren_lang$node$HttpClient$ReceivedChunk,
              request,
              _HttpClient_formatResponse(
                res,
                new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength)
              )
            )
          )
        );
      });

      res.on("error", (e) => {
        _Scheduler_rawSpawn(cleanup(request));
        _Scheduler_rawSpawn(
          sendToApp(
            $gren_lang$node$HttpClient$Error(
              $gren_lang$node$HttpClient$UnknownError("problem with request: " + e.message)
            )
          )
        );
      });

      res.on("end", () => {
        _Scheduler_rawSpawn(cleanup(request));
        _Scheduler_rawSpawn(sendToApp($gren_lang$node$HttpClient$Done));
      });
    });

    kernelRequest.request.end(() => {
      return callback(_Scheduler_succeed({}));
    });
  });
});

var _HttpClient_abort = function (kernelRequest) {
  return _Scheduler_binding(function (callback) {
    if (!kernelRequest.request.writableEnded) {
      kernelRequest.request.destroy(_HttpClient_CustomAbortError);
    } else if (
      kernelRequest.response &&
      kernelRequest.response.complete === false
    ) {
      kernelRequest.response.destroy(_HttpClient_CustomAbortError);
    }

    return callback(_Scheduler_succeed({}));
  });
};

// HELPERS

var _HttpClient_dictToObject = F3(function (key, value, obj) {
  obj[key] = value;
  return obj;
});

var _HttpClient_extractRequestBody = function (config) {
  switch (config.bodyType) {
    case "EMPTY":
      return null;
    case "STRING":
      return config.body.a;
    case "BYTES":
      return _HttpClient_prepBytes(config.body.a);
  }
};

var _HttpClient_prepBytes = function (bytes) {
  return new Uint8Array(bytes.buffer);
};

var _HttpClient_CustomAbortError = new Error();

var _HttpClient_CustomTimeoutError = new Error();

var _HttpClient_formatResponse = function (res, data) {
  let headerDict = $gren_lang$core$Dict$empty;
  for (const [key, value] of Object.entries(res.headersDistinct)) {
    headerDict = A3($gren_lang$core$Dict$set, key.toLowerCase(), value, headerDict);
  }

  return {
    statusCode: res.statusCode,
    statusText: res.statusMessage,
    headers: headerDict,
    data: data,
  };
};
var $gren_lang$node$HttpClient$Aborted = { $: 'Aborted' };
var $gren_lang$node$HttpClient$BadHeaders = { $: 'BadHeaders' };
var $gren_lang$node$HttpClient$BadStatus = function (a) {
	return { $: 'BadStatus', a: a };
};
var $gren_lang$node$HttpClient$BadUrl = function (a) {
	return { $: 'BadUrl', a: a };
};
var $gren_lang$node$HttpClient$Done = { $: 'Done' };
var $gren_lang$node$HttpClient$Error = function (a) {
	return { $: 'Error', a: a };
};
var $gren_lang$node$HttpClient$ReceivedChunk$ = function (a, b) {
	return { $: 'ReceivedChunk', a: a, b: b };
};
var $gren_lang$node$HttpClient$ReceivedChunk = F2($gren_lang$node$HttpClient$ReceivedChunk$);
var $gren_lang$node$HttpClient$SentChunk = function (a) {
	return { $: 'SentChunk', a: a };
};
var $gren_lang$node$HttpClient$Timeout = { $: 'Timeout' };
var $gren_lang$node$HttpClient$UnexpectedResponseBody = function (a) {
	return { $: 'UnexpectedResponseBody', a: a };
};
var $gren_lang$node$HttpClient$UnknownError = function (a) {
	return { $: 'UnknownError', a: a };
};
var $gren_lang$core$Json$Decode$decodeString = _Json_runOnString;
var $gren_lang$node$HttpClient$bodyTypeAsString = function(body) {
	switch (body.$) {
		case 'BodyEmpty':
			return 'EMPTY';
		case 'BodyString':
			return 'STRING';
		default:
			return 'BYTES';
	}
};
var $gren_lang$node$HttpClient$expectTypeAsString = function(expect) {
	switch (expect.$) {
		case 'ExpectNothing':
			return 'NOTHING';
		case 'ExpectAnything':
			return 'ANYTHING';
		case 'ExpectString':
			return 'STRING';
		case 'ExpectJson':
			return 'JSON';
		default:
			return 'BYTES';
	}
};
var $gren_lang$node$HttpServer$methodToString = function(method) {
	switch (method.$) {
		case 'GET':
			return 'GET';
		case 'HEAD':
			return 'HEAD';
		case 'POST':
			return 'POST';
		case 'PUT':
			return 'PUT';
		case 'DELETE':
			return 'DELETE';
		case 'CONNECT':
			return 'CONNECT';
		case 'TRACE':
			return 'TRACE';
		case 'PATCH':
			return 'PATCH';
		default:
			var value = method.a;
			return value;
	}
};
var $gren_lang$node$HttpClient$kernelRequestConfig$ = function(permission, config) {
	var actualUrl = function () {
		if (permission.$ === 'AnyPermission') {
			return config.url;
		} else {
			var prefix = permission.a;
			return _Utils_ap(prefix, config.url);
		}
	}();
	return { body: config.body, bodyType: $gren_lang$node$HttpClient$bodyTypeAsString(config.body), expect: config.expect, expectType: $gren_lang$node$HttpClient$expectTypeAsString(config.expect), headers: config.headers, method: $gren_lang$node$HttpServer$methodToString(config.method), timeout: config.timeout, url: actualUrl };
};
var $gren_lang$node$HttpClient$kernelRequestConfig = F2($gren_lang$node$HttpClient$kernelRequestConfig$);
var $gren_lang$node$HttpClient$send$ = function(permission, config) {
	return _HttpClient_request($gren_lang$node$HttpClient$kernelRequestConfig$(permission, config));
};
var $gren_lang$node$HttpClient$send = F2($gren_lang$node$HttpClient$send$);
var $author$project$Main$downloadBinary$ = function(permission, url) {
	return $gren_lang$node$HttpClient$send$(permission, $gren_lang$node$HttpClient$expectBytes($gren_lang$node$HttpClient$get(url)));
};
var $author$project$Main$downloadBinary = F2($author$project$Main$downloadBinary$);
var $gren_lang$node$FileSystem$errorToString = function(_v0) {
	var message = _v0.b;
	return message;
};
var $gren_lang$node$HttpClient$errorToString = function(err) {
	switch (err.$) {
		case 'Timeout':
			return 'Timeout';
		case 'BadUrl':
			var url = err.a;
			return _Utils_ap('Bad URL: ', url);
		case 'BadHeaders':
			return 'Bad headers: one or more of your headers contains invalid characters.';
		case 'BadStatus':
			var res = err.a;
			return _Utils_ap('Bad status: ', _Utils_ap($gren_lang$core$String$fromInt(res.statusCode), _Utils_ap(' - ', res.statusText)));
		case 'UnexpectedResponseBody':
			var message = err.a;
			return _Utils_ap('Unexpected response body: ', message);
		default:
			var debugStr = err.a;
			return _Utils_ap('Unknown error: ', debugStr);
	}
};
var $gren_lang$node$FileSystem$makeDirectory$ = function(_v0, options, path) {
	return A2(_FileSystem_makeDirectory, options, path);
};
var $gren_lang$node$FileSystem$makeDirectory = F3($gren_lang$node$FileSystem$makeDirectory$);
var $gren_lang$core$Array$dropLast$ = function(n, array) {
	return A3($gren_lang$core$Array$slice, 0, $gren_lang$core$Array$length(array) - n, array);
};
var $gren_lang$core$Array$dropLast = F2($gren_lang$core$Array$dropLast$);
var $gren_lang$core$Array$get = _Array_get;
var $gren_lang$core$Basics$negate = function(n) {
	return -n;
};
var $gren_lang$core$Array$last = function(array) {
	return A2($gren_lang$core$Array$get, -1, array);
};
var $gren_lang$core$Array$popLast = function(array) {
	var _v0 = $gren_lang$core$Array$last(array);
	if (_v0.$ === 'Just') {
		var value = _v0.a;
		return $gren_lang$core$Maybe$Just({ initial: $gren_lang$core$Array$dropLast$(1, array), last: value });
	} else {
		return $gren_lang$core$Maybe$Nothing;
	}
};
var $gren_lang$node$FileSystem$Path$parentPath = function(path) {
	var _v0 = $gren_lang$core$Array$popLast(path.directory);
	if (_v0.$ === 'Nothing') {
		return _Utils_eq($gren_lang$node$FileSystem$Path$filenameWithExtension(path), '') ? $gren_lang$core$Maybe$Nothing : $gren_lang$core$Maybe$Just(_Utils_update(path, { extension: '', filename: '' }));
	} else {
		var _v1 = _v0.a;
		var last = _v1.last;
		var initial = _v1.initial;
		var _v2 = function () {
			var _v3 = A2($gren_lang$core$String$split, '.', last);
			if (_v3.length === 2) {
				var file = _v3[0];
				var ext = _v3[1];
				return { extension: ext, filename: file };
			} else {
				return { extension: '', filename: last };
			}
		}();
		var filename = _v2.filename;
		var extension = _v2.extension;
		return $gren_lang$core$Maybe$Just(_Utils_update(path, { directory: initial, extension: extension, filename: filename }));
	}
};
var $gren_lang$node$ChildProcess$MergeWithEnvironmentVariables = function (a) {
	return { $: 'MergeWithEnvironmentVariables', a: a };
};
var $gren_lang$node$ChildProcess$DefaultShell = { $: 'DefaultShell' };
var $gren_lang$node$ChildProcess$InheritEnvironmentVariables = { $: 'InheritEnvironmentVariables' };
var $gren_lang$node$ChildProcess$InheritWorkingDirectory = { $: 'InheritWorkingDirectory' };
var $gren_lang$node$ChildProcess$Integrated = { $: 'Integrated' };
var $gren_lang$node$ChildProcess$NoLimit = { $: 'NoLimit' };
var $gren_lang$node$ChildProcess$defaultSpawnOptions = { connection: $gren_lang$node$ChildProcess$Integrated, environmentVariables: $gren_lang$node$ChildProcess$InheritEnvironmentVariables, runDuration: $gren_lang$node$ChildProcess$NoLimit, shell: $gren_lang$node$ChildProcess$DefaultShell, workingDirectory: $gren_lang$node$ChildProcess$InheritWorkingDirectory };
var $gren_lang$core$Dict$singleton$ = function(key, value) {
	return $gren_lang$core$Dict$RBNode_gren_builtin$($gren_lang$core$Dict$Black, key, value, $gren_lang$core$Dict$RBEmpty_gren_builtin, $gren_lang$core$Dict$RBEmpty_gren_builtin);
};
var $gren_lang$core$Dict$singleton = F2($gren_lang$core$Dict$singleton$);


var bufferNs = require("node:buffer");
var process = require("node:process");
var childProcess = require("node:child_process");

var _ChildProcess_run = function (options) {
  return _Scheduler_binding(function (callback) {
    var workingDir = options.workingDirectory;
    var env = options.environmentVariables;
    var shell = options.shell;

    childProcess.execFile(
      options.program,
      options._arguments,
      {
        encoding: "buffer",
        timeout: options.runDuration,
        cwd: _ChildProcess_handleCwd(workingDir),
        env: _ChildProcess_handleEnv(env),
        timeout: options.runDuration,
        maxBuffer: options.maximumBytesWrittenToStreams,
        shell: _ChildProcess_handleShell(shell),
      },
      function (err, stdout, stderr) {
        if (err == null) {
          callback(
            _Scheduler_succeed({
              stdout: new DataView(
                stdout.buffer,
                stdout.byteOffset,
                stdout.byteLength
              ),
              stderr: new DataView(
                stderr.buffer,
                stderr.byteOffset,
                stderr.byteLength
              ),
            })
          );
        } else {
          callback(
            _Scheduler_fail({
              exitCode:
                typeof err.errno === "undefined" ? err.code : err.errno,
              stdout: new DataView(
                stdout.buffer,
                stdout.byteOffset,
                stdout.byteLength
              ),
              stderr: new DataView(
                stderr.buffer,
                stderr.byteOffset,
                stderr.byteLength
              ),
            })
          );
        }
      }
    );
  });
};

var _ChildProcess_spawn = function (options) {
  return _Scheduler_binding(function (callback) {
    var workingDir = options.workingDirectory;
    var env = options.environmentVariables;
    var shell = options.shell;

    var subproc = childProcess.spawn(options.program, options._arguments, {
      cwd: _ChildProcess_handleCwd(workingDir),
      env: _ChildProcess_handleEnv(env),
      timeout: options.runDuration,
      shell: _ChildProcess_handleShell(shell),
      stdio: options.connection === 0 ? "inherit" : "ignore",
      detached: options.connection === 2 && process.platform === "win32",
    });

    if (options.connection === 2) {
      subproc.unref();
    }

    return function () {
      subproc.kill();
    };
  });
};

function _ChildProcess_handleCwd(cwd) {
  return cwd.inherit ? process.cwd() : cwd.override;
}

function _ChildProcess_handleEnv(env) {
  return env.option === 0
    ? process.env
    : env.option === 1
    ? _Utils_update(process.env, _ChildProcess_dictToObj(env.value))
    : _ChildProcess_dictToObj(env.value);
}

function _ChildProcess_handleShell(shell) {
  return shell.choice === 0
    ? false
    : shell.choice === 1
    ? true
    : shell.value;
}

function _ChildProcess_dictToObj(dict) {
  return A3(
    $gren_lang$core$Dict$foldl,
    F3(function (key, value, acc) {
      acc[key] = value;
      return acc;
    }),
    {},
    dict
  );
}
var $gren_lang$core$Basics$gt = _Utils_gt;
var $gren_lang$core$Basics$max$ = function(x, y) {
	return (_Utils_cmp(x, y) > 0) ? x : y;
};
var $gren_lang$core$Basics$max = F2($gren_lang$core$Basics$max$);
var $gren_lang$core$Process$spawn = _Scheduler_spawn;
var $gren_lang$node$ChildProcess$spawn$ = function(_v0, program, _arguments, opts) {
	return $gren_lang$core$Process$spawn(_ChildProcess_spawn({ _arguments: _arguments, connection: function () {
				var _v1 = opts.connection;
				switch (_v1.$) {
					case 'Integrated':
						return 0;
					case 'Ignored':
						return 1;
					default:
						return 2;
				}
			}(), environmentVariables: function () {
				var _v2 = opts.environmentVariables;
				switch (_v2.$) {
					case 'InheritEnvironmentVariables':
						return { option: 0, value: $gren_lang$core$Dict$empty };
					case 'MergeWithEnvironmentVariables':
						var value = _v2.a;
						return { option: 1, value: value };
					default:
						var value = _v2.a;
						return { option: 2, value: value };
				}
			}(), program: program, runDuration: function () {
				var _v3 = opts.runDuration;
				if (_v3.$ === 'NoLimit') {
					return 0;
				} else {
					var ms = _v3.a;
					return $gren_lang$core$Basics$max$(0, ms);
				}
			}(), shell: function () {
				var _v4 = opts.shell;
				switch (_v4.$) {
					case 'NoShell':
						return { choice: 0, value: '' };
					case 'DefaultShell':
						return { choice: 1, value: '' };
					default:
						var value = _v4.a;
						return { choice: 2, value: value };
				}
			}(), workingDirectory: function () {
				var _v5 = opts.workingDirectory;
				if (_v5.$ === 'InheritWorkingDirectory') {
					return { inherit: true, override: '' };
				} else {
					var value = _v5.a;
					return { inherit: false, override: value };
				}
			}() }));
};
var $gren_lang$node$ChildProcess$spawn = F4($gren_lang$node$ChildProcess$spawn$);
var $author$project$Main$runCompiler = function(model) {
	var colorEnvVar = model.useColor ? $gren_lang$core$Dict$singleton$('FORCE_COLOR', '1') : $gren_lang$core$Dict$singleton$('NO_COLOR', '1');
	return $gren_lang$node$ChildProcess$spawn$(model.cpPermission, model.pathToString(model.localPath), model.args, _Utils_update($gren_lang$node$ChildProcess$defaultSpawnOptions, { environmentVariables: $gren_lang$node$ChildProcess$MergeWithEnvironmentVariables(colorEnvVar) }));
};
var $gren_lang$node$FileSystem$writeFile$ = function(_v0, bytes, path) {
	return A2(_FileSystem_writeFile, bytes, path);
};
var $gren_lang$node$FileSystem$writeFile = F3($gren_lang$node$FileSystem$writeFile$);
var $author$project$Main$update$ = function(msg, model) {
	switch (msg.$) {
		case 'ExistanceChecked':
			if (msg.a.$ === 'Err') {
				return { command: function () {
					var _v1 = model.remotePath;
					if (_v1.$ === 'Just') {
						var remotePath = _v1.a;
						return $gren_lang$core$Task$attempt$($author$project$Main$CompilerDownloaded, A2($gren_lang$core$Task$andThen, function(_v2) {
									return $author$project$Main$downloadBinary$(model.httpPermission, remotePath);
								}, $gren_lang$node$Stream$sendLine$(model.stdout, _Utils_ap('Compiler not found at ', _Utils_ap(model.pathToString(model.localPath), '. Downloading...')))));
					} else {
						return $gren_lang$core$Task$execute($gren_lang$node$Stream$sendLine$(model.stderr, _Utils_ap('Compiler not found at ', model.pathToString(model.localPath))));
					}
				}(), model: model };
			} else {
				return { command: $gren_lang$core$Task$execute($author$project$Main$runCompiler(model)), model: model };
			}
		case 'CompilerDownloaded':
			if (msg.a.$ === 'Err') {
				if (msg.a.a.$ === 'BadStatus') {
					var err = msg.a.a;
					var res = err.a;
					if (_Utils_eq(res.statusCode, 302)) {
						var _v3 = A2($gren_lang$core$Dict$get, 'location', res.headers);
						if ((_v3.$ === 'Just') && (_v3.a.length === 1)) {
							var location = _v3.a[0];
							return { command: $gren_lang$core$Task$attempt$($author$project$Main$CompilerDownloaded, $author$project$Main$downloadBinary$(model.httpPermission, location)), model: model };
						} else {
							return { command: $gren_lang$core$Task$execute($gren_lang$node$Stream$sendLine$(model.stderr, 'Missing, or vague, \'location\' header in 302 response from server.')), model: model };
						}
					} else {
						return { command: $gren_lang$core$Task$execute($gren_lang$node$Stream$sendLine$(model.stderr, $gren_lang$node$HttpClient$errorToString(err))), model: model };
					}
				} else {
					var err = msg.a.a;
					return { command: $gren_lang$core$Task$execute($gren_lang$node$Stream$sendLine$(model.stderr, $gren_lang$node$HttpClient$errorToString(err))), model: model };
				}
			} else {
				var res = msg.a.a;
				var cacheFolder = $gren_lang$core$Maybe$withDefault$($gren_lang$node$FileSystem$Path$empty, $gren_lang$node$FileSystem$Path$parentPath(model.localPath));
				return { command: $gren_lang$core$Task$attempt$($author$project$Main$CompilerInstalled, A2($gren_lang$core$Task$andThen, function(_v5) {
							return $gren_lang$node$Stream$sendLine$(model.stdout, 'Downloaded');
						}, A2($gren_lang$core$Task$andThen, A2($gren_lang$node$FileSystem$changeAccess, model.fsPermission, { group: [ $gren_lang$node$FileSystem$Read, $gren_lang$node$FileSystem$Execute ], others: [ $gren_lang$node$FileSystem$Read, $gren_lang$node$FileSystem$Execute ], owner: [ $gren_lang$node$FileSystem$Read, $gren_lang$node$FileSystem$Write, $gren_lang$node$FileSystem$Execute ] }), A2($gren_lang$core$Task$andThen, function(_v4) {
									return $gren_lang$node$FileSystem$writeFile$(model.fsPermission, res.data, model.localPath);
								}, $gren_lang$node$FileSystem$makeDirectory$(model.fsPermission, { recursive: true }, cacheFolder))))), model: model };
			}
		default:
			if (msg.a.$ === 'Err') {
				var fsErr = msg.a.a;
				return { command: $gren_lang$core$Task$execute($gren_lang$node$Stream$sendLine$(model.stderr, _Utils_ap('Failed to install binary after download, due to error: ', $gren_lang$node$FileSystem$errorToString(fsErr)))), model: model };
			} else {
				return { command: $gren_lang$core$Task$execute($author$project$Main$runCompiler(model)), model: model };
			}
	}
};
var $author$project$Main$update = F2($author$project$Main$update$);
var $author$project$Main$main = $gren_lang$node$Node$defineProgram({ init: $author$project$Main$init, subscriptions: function(_v0) {
		return $gren_lang$core$Platform$Sub$none;
	}, update: $author$project$Main$update });
_Platform_export({'Main':{'init':$author$project$Main$main($gren_lang$core$Json$Decode$succeed({  }))(0)}});}(this.module ? this.module.exports : this));
this.Gren.Main.init({});
}
catch (e)
{
console.error(e);
}

//# sourceMappingURL=data:application/json;base64,ewogICAgInZlcnNpb24iOiAzLAogICAgInNvdXJjZXMiOiBbCiAgICAgICAgIlRhc2siLAogICAgICAgICJCYXNpY3MiLAogICAgICAgICJEaWN0IiwKICAgICAgICAiQXJyYXkiLAogICAgICAgICJTZXQiLAogICAgICAgICJTdHJpbmciLAogICAgICAgICJKc29uLkVuY29kZSIsCiAgICAgICAgIkpzb24uRGVjb2RlIiwKICAgICAgICAiQ2hhciIsCiAgICAgICAgIlJlc3VsdCIsCiAgICAgICAgIk5vZGUiLAogICAgICAgICJQbGF0Zm9ybSIsCiAgICAgICAgIlBsYXRmb3JtLlN1YiIsCiAgICAgICAgIlBsYXRmb3JtLkNtZCIsCiAgICAgICAgIkluaXQiLAogICAgICAgICJUaW1lIiwKICAgICAgICAiRmlsZVN5c3RlbSIsCiAgICAgICAgIk1haW4iLAogICAgICAgICJGaWxlU3lzdGVtLlBhdGgiLAogICAgICAgICJDaGlsZFByb2Nlc3MiLAogICAgICAgICJIdHRwQ2xpZW50IiwKICAgICAgICAiVGVybWluYWwiLAogICAgICAgICJNYXliZSIsCiAgICAgICAgIkJ5dGVzLkVuY29kZSIsCiAgICAgICAgIkJ5dGVzIiwKICAgICAgICAiU3RyZWFtIiwKICAgICAgICAiSHR0cFNlcnZlciIsCiAgICAgICAgIlByb2Nlc3MiCiAgICBdLAogICAgInNvdXJjZXNDb250ZW50IjogWwogICAgICAgICJlZmZlY3QgbW9kdWxlIFRhc2sgd2hlcmUgeyBjb21tYW5kID0gTXlDbWQgfSBleHBvc2luZ1xuICAgICggVGFzaywgcGVyZm9ybSwgYXR0ZW1wdCwgZXhlY3V0ZVxuICAgICwgYW5kVGhlbiwgYXdhaXQsIHN1Y2NlZWQsIGZhaWwsIHNlcXVlbmNlXG4gICAgLCBtYXAsIG1hcDIsIG1hcDMsIG1hcDQsIG1hcDVcbiAgICAsIG9uRXJyb3IsIG1hcEVycm9yXG4gICAgKVxuXG57LXwgVGFza3MgbWFrZSBpdCBlYXN5IHRvIGRlc2NyaWJlIGFzeW5jaHJvbm91cyBvcGVyYXRpb25zIHRoYXQgbWF5IGZhaWwsIGxpa2VcbkhUVFAgcmVxdWVzdHMgb3Igd3JpdGluZyB0byBhIGRhdGFiYXNlLlxuXG5cbkBkb2NzIFRhc2ssIHBlcmZvcm0sIGF0dGVtcHQsIGV4ZWN1dGVcblxuXG4jIyBDaGFpbnNcblxuQGRvY3MgYW5kVGhlbiwgYXdhaXQsIHN1Y2NlZWQsIGZhaWwsIHNlcXVlbmNlXG5cblxuIyMgTWFwc1xuXG5AZG9jcyBtYXAsIG1hcDIsIG1hcDMsIG1hcDQsIG1hcDVcblxuXG4jIyBFcnJvcnNcblxuQGRvY3Mgb25FcnJvciwgbWFwRXJyb3JcblxuLX1cblxuaW1wb3J0IEFycmF5IGV4cG9zaW5nIChBcnJheSlcbmltcG9ydCBCYXNpY3MgZXhwb3NpbmcgKCg8PCksICh8PiksIE5ldmVyKVxuaW1wb3J0IEdyZW4uS2VybmVsLlNjaGVkdWxlclxuaW1wb3J0IE1heWJlIGV4cG9zaW5nIChNYXliZSguLikpXG5pbXBvcnQgUGxhdGZvcm1cbmltcG9ydCBQbGF0Zm9ybS5DbWQgZXhwb3NpbmcgKENtZClcbmltcG9ydCBSZXN1bHQgZXhwb3NpbmcgKFJlc3VsdCguLikpXG5cblxuey18IEhlcmUgYXJlIHNvbWUgY29tbW9uIHRhc2tzOlxuXG4gIC0gW2Bub3cgOiBUYXNrIHggUG9zaXhgXShUaW1lI25vdylcbiAgLSBbYGZvY3VzIDogU3RyaW5nIC0+IFRhc2sgRXJyb3Ige31gXVtmb2N1c11cbiAgLSBbYHNsZWVwIDogRmxvYXQgLT4gVGFzayB4IHt9YF0oUHJvY2VzcyNzbGVlcClcblxuW2ZvY3VzXTogL3BhY2thZ2UvZ3Jlbi1sYW5nL2Jyb3dzZXIvbGF0ZXN0L21vZHVsZS9Ccm93c2VyLkRvbSNmb2N1c1xuXG5JbiBlYWNoIGNhc2Ugd2UgaGF2ZSBhIGBUYXNrYCB0aGF0IHdpbGwgcmVzb2x2ZSBzdWNjZXNzZnVsbHkgd2l0aCBhbiBgYWAgdmFsdWVcbm9yIHVuc3VjY2Vzc2Z1bGx5IHdpdGggYW4gYHhgIHZhbHVlLiBTbyBgQnJvd3Nlci5Eb20uZm9jdXNgIHdlIG1heSBmYWlsIHdpdGggYW5cbmBFcnJvcmAgaWYgdGhlIGdpdmVuIElEIGRvZXMgbm90IGV4aXN0LiBXaGVyZWFzIGBUaW1lLm5vd2AgbmV2ZXIgZmFpbHMgc29cbkkgY2Fubm90IGJlIG1vcmUgc3BlY2lmaWMgdGhhbiBgeGAuIE5vIHN1Y2ggdmFsdWUgd2lsbCBldmVyIGV4aXN0ISBJbnN0ZWFkIGl0XG5hbHdheXMgc3VjY2VlZHMgd2l0aCB0aGUgY3VycmVudCBQT1NJWCB0aW1lLlxuXG5Nb3JlIGdlbmVyYWxseSBhIHRhc2sgaXMgYSBfZGVzY3JpcHRpb25fIG9mIHdoYXQgeW91IG5lZWQgdG8gZG8uIExpa2UgYSB0b2RvXG5saXN0LiBPciBsaWtlIGEgZ3JvY2VyeSBsaXN0LiBPciBsaWtlIEdpdEh1YiBpc3N1ZXMuIFNvIHNheWluZyBcInRoZSB0YXNrIGlzXG50byB0ZWxsIG1lIHRoZSBjdXJyZW50IFBPU0lYIHRpbWVcIiBkb2VzIG5vdCBjb21wbGV0ZSB0aGUgdGFzayEgWW91IG5lZWRcbltgcGVyZm9ybWBdKCNwZXJmb3JtKSB0YXNrcyBvciBbYGF0dGVtcHRgXSgjYXR0ZW1wdCkgdGFza3MuXG5cbi19XG50eXBlIGFsaWFzIFRhc2sgeCBhID1cbiAgICBQbGF0Zm9ybS5UYXNrIHggYVxuXG5cblxuLS0gQkFTSUNTXG5cblxuey18IEEgdGFzayB0aGF0IHN1Y2NlZWRzIGltbWVkaWF0ZWx5IHdoZW4gcnVuLiBJdCBpcyB1c3VhbGx5IHVzZWQgd2l0aFxuW2BhbmRUaGVuYF0oI2FuZFRoZW4pLiBZb3UgY2FuIHVzZSBpdCBsaWtlIGBtYXBgIGlmIHlvdSB3YW50OlxuXG4gICAgaW1wb3J0IFRpbWVcblxuXG4gICAgdGltZUluTWlsbGlzIDogVGFzayB4IEludFxuICAgIHRpbWVJbk1pbGxpcyA9XG4gICAgICAgIFRpbWUubm93XG4gICAgICAgICAgICB8PiBhbmRUaGVuIChcXHQgLT4gc3VjY2VlZCAoVGltZS5wb3NpeFRvTWlsbGlzIHQpKVxuXG4tfVxuc3VjY2VlZCA6IGEgLT4gVGFzayB4IGFcbnN1Y2NlZWQgPVxuICAgIEdyZW4uS2VybmVsLlNjaGVkdWxlci5zdWNjZWVkXG5cblxuey18IEEgdGFzayB0aGF0IGZhaWxzIGltbWVkaWF0ZWx5IHdoZW4gcnVuLiBMaWtlIHdpdGggYHN1Y2NlZWRgLCB0aGlzIGNhbiBiZVxudXNlZCB3aXRoIGBhbmRUaGVuYCB0byBjaGVjayBvbiB0aGUgb3V0Y29tZSBvZiBhbm90aGVyIHRhc2suXG5cbiAgICB0eXBlIEVycm9yXG4gICAgICAgID0gTm90Rm91bmRcblxuICAgIG5vdEZvdW5kIDogVGFzayBFcnJvciBhXG4gICAgbm90Rm91bmQgPVxuICAgICAgICBmYWlsIE5vdEZvdW5kXG5cbi19XG5mYWlsIDogeCAtPiBUYXNrIHggYVxuZmFpbCA9XG4gICAgR3Jlbi5LZXJuZWwuU2NoZWR1bGVyLmZhaWxcblxuXG5cbi0tIE1BUFBJTkdcblxuXG57LXwgVHJhbnNmb3JtIGEgdGFzay4gTWF5YmUgeW91IHdhbnQgdG8gdXNlIFtgVGltZWBdW3RpbWVdIHRvIGZpZ3VyZVxub3V0IHdoYXQgdGltZSBpdCB3aWxsIGJlIGluIG9uZSBob3VyOlxuXG4gICAgaW1wb3J0IFRhc2sgZXhwb3NpbmcgKFRhc2spXG4gICAgaW1wb3J0IFRpbWVcblxuXG4gICAgdGltZUluT25lSG91ciA6IFRhc2sgeCBUaW1lLlBvc2l4XG4gICAgdGltZUluT25lSG91ciA9XG4gICAgICAgIFRhc2subWFwIGFkZEFuSG91ciBUaW1lLm5vd1xuXG4gICAgYWRkQW5Ib3VyIDogVGltZS5Qb3NpeCAtPiBUaW1lLlBvc2l4XG4gICAgYWRkQW5Ib3VyIHRpbWUgPVxuICAgICAgICBUaW1lLm1pbGxpc1RvUG9zaXggKFRpbWUucG9zaXhUb01pbGxpcyB0aW1lICsgNjAgKiA2MCAqIDEwMDApXG5cblt0aW1lXTogVGltZVxuXG4tfVxubWFwIDogKGEgLT4gYikgLT4gVGFzayB4IGEgLT4gVGFzayB4IGJcbm1hcCBmdW5jIHRhc2tBID1cbiAgICB0YXNrQVxuICAgICAgICB8PiBhbmRUaGVuIChcXGEgLT4gc3VjY2VlZCAoZnVuYyBhKSlcblxuXG57LXwgUHV0IHRoZSByZXN1bHRzIG9mIHR3byB0YXNrcyB0b2dldGhlci4gRm9yIGV4YW1wbGUsIGlmIHdlIHdhbnRlZCB0byBrbm93XG50aGUgY3VycmVudCBtb250aCwgd2UgY291bGQgdXNlIFtgVGltZWBdW3RpbWVdIHRvIGFzazpcblxuICAgIGltcG9ydCBUYXNrIGV4cG9zaW5nIChUYXNrKVxuICAgIGltcG9ydCBUaW1lXG5cblxuICAgIGdldE1vbnRoIDogVGFzayB4IEludFxuICAgIGdldE1vbnRoID1cbiAgICAgICAgVGFzay5tYXAyIFRpbWUudG9Nb250aCBUaW1lLmhlcmUgVGltZS5ub3dcblxuKipOb3RlOioqIFNheSB3ZSB3ZXJlIGRvaW5nIEhUVFAgcmVxdWVzdHMgaW5zdGVhZC4gYG1hcDJgIGRvZXMgZWFjaCB0YXNrIGluXG5vcmRlciwgc28gaXQgd291bGQgdHJ5IHRoZSBmaXJzdCByZXF1ZXN0IGFuZCBvbmx5IGNvbnRpbnVlIGFmdGVyIGl0IHN1Y2NlZWRzLlxuSWYgaXQgZmFpbHMsIHRoZSB3aG9sZSB0aGluZyBmYWlscyFcblxuW3RpbWVdOiBUaW1lXG5cbi19XG5tYXAyIDogKGEgLT4gYiAtPiByZXN1bHQpIC0+IFRhc2sgeCBhIC0+IFRhc2sgeCBiIC0+IFRhc2sgeCByZXN1bHRcbm1hcDIgZnVuYyB0YXNrQSB0YXNrQiA9XG4gICAgdGFza0FcbiAgICAgICAgfD4gYW5kVGhlblxuICAgICAgICAgICAgKFxcYSAtPlxuICAgICAgICAgICAgICAgIHRhc2tCXG4gICAgICAgICAgICAgICAgICAgIHw+IGFuZFRoZW4gKFxcYiAtPiBzdWNjZWVkIChmdW5jIGEgYikpXG4gICAgICAgICAgICApXG5cblxuey18IC19XG5tYXAzIDogKGEgLT4gYiAtPiBjIC0+IHJlc3VsdCkgLT4gVGFzayB4IGEgLT4gVGFzayB4IGIgLT4gVGFzayB4IGMgLT4gVGFzayB4IHJlc3VsdFxubWFwMyBmdW5jIHRhc2tBIHRhc2tCIHRhc2tDID1cbiAgICB0YXNrQVxuICAgICAgICB8PiBhbmRUaGVuXG4gICAgICAgICAgICAoXFxhIC0+XG4gICAgICAgICAgICAgICAgdGFza0JcbiAgICAgICAgICAgICAgICAgICAgfD4gYW5kVGhlblxuICAgICAgICAgICAgICAgICAgICAgICAgKFxcYiAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRhc2tDXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHw+IGFuZFRoZW4gKFxcYyAtPiBzdWNjZWVkIChmdW5jIGEgYiBjKSlcbiAgICAgICAgICAgICAgICAgICAgICAgIClcbiAgICAgICAgICAgIClcblxuXG57LXwgLX1cbm1hcDQgOiAoYSAtPiBiIC0+IGMgLT4gZCAtPiByZXN1bHQpIC0+IFRhc2sgeCBhIC0+IFRhc2sgeCBiIC0+IFRhc2sgeCBjIC0+IFRhc2sgeCBkIC0+IFRhc2sgeCByZXN1bHRcbm1hcDQgZnVuYyB0YXNrQSB0YXNrQiB0YXNrQyB0YXNrRCA9XG4gICAgdGFza0FcbiAgICAgICAgfD4gYW5kVGhlblxuICAgICAgICAgICAgKFxcYSAtPlxuICAgICAgICAgICAgICAgIHRhc2tCXG4gICAgICAgICAgICAgICAgICAgIHw+IGFuZFRoZW5cbiAgICAgICAgICAgICAgICAgICAgICAgIChcXGIgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB0YXNrQ1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB8PiBhbmRUaGVuXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoXFxjIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdGFza0RcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfD4gYW5kVGhlbiAoXFxkIC0+IHN1Y2NlZWQgKGZ1bmMgYSBiIGMgZCkpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICApXG5cblxuey18IC19XG5tYXA1IDogKGEgLT4gYiAtPiBjIC0+IGQgLT4gZSAtPiByZXN1bHQpIC0+IFRhc2sgeCBhIC0+IFRhc2sgeCBiIC0+IFRhc2sgeCBjIC0+IFRhc2sgeCBkIC0+IFRhc2sgeCBlIC0+IFRhc2sgeCByZXN1bHRcbm1hcDUgZnVuYyB0YXNrQSB0YXNrQiB0YXNrQyB0YXNrRCB0YXNrRSA9XG4gICAgdGFza0FcbiAgICAgICAgfD4gYW5kVGhlblxuICAgICAgICAgICAgKFxcYSAtPlxuICAgICAgICAgICAgICAgIHRhc2tCXG4gICAgICAgICAgICAgICAgICAgIHw+IGFuZFRoZW5cbiAgICAgICAgICAgICAgICAgICAgICAgIChcXGIgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB0YXNrQ1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB8PiBhbmRUaGVuXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAoXFxjIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgdGFza0RcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfD4gYW5kVGhlblxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKFxcZCAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRhc2tFXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHw+IGFuZFRoZW4gKFxcZSAtPiBzdWNjZWVkIChmdW5jIGEgYiBjIGQgZSkpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICApXG5cblxuey18IFN0YXJ0IHdpdGggYSBsaXN0IG9mIHRhc2tzLCBhbmQgdHVybiB0aGVtIGludG8gYSBzaW5nbGUgdGFzayB0aGF0IHJldHVybnMgYVxubGlzdC4gVGhlIHRhc2tzIHdpbGwgYmUgcnVuIGluIG9yZGVyIG9uZS1ieS1vbmUgYW5kIGlmIGFueSB0YXNrIGZhaWxzIHRoZSB3aG9sZVxuc2VxdWVuY2UgZmFpbHMuXG5cbiAgICBzZXF1ZW5jZSBbIHN1Y2NlZWQgMSwgc3VjY2VlZCAyIF0gPT0gc3VjY2VlZCBbIDEsIDIgXVxuXG4tfVxuc2VxdWVuY2UgOiBBcnJheSAoVGFzayB4IGEpIC0+IFRhc2sgeCAoQXJyYXkgYSlcbnNlcXVlbmNlIHRhc2tzID1cbiAgICBBcnJheS5mb2xkciAobWFwMiBBcnJheS5wdXNoRmlyc3QpIChzdWNjZWVkIFtdKSB0YXNrc1xuXG5cblxuLS0gQ0hBSU5JTkdcblxuXG57LXwgQ2hhaW4gdG9nZXRoZXIgYSB0YXNrIGFuZCBhIGNhbGxiYWNrLiBUaGUgZmlyc3QgdGFzayB3aWxsIHJ1biwgYW5kIGlmIGl0IGlzXG5zdWNjZXNzZnVsLCB5b3UgZ2l2ZSB0aGUgcmVzdWx0IHRvIHRoZSBjYWxsYmFjayByZXN1bHRpbmcgaW4gYW5vdGhlciB0YXNrLiBUaGlzXG50YXNrIHRoZW4gZ2V0cyBydW4uIFdlIGNvdWxkIHVzZSB0aGlzIHRvIG1ha2UgYSB0YXNrIHRoYXQgcmVzb2x2ZXMgYW4gaG91ciBmcm9tXG5ub3c6XG5cblxuICAgIGltcG9ydCBQcm9jZXNzXG4gICAgaW1wb3J0IFRpbWVcblxuICAgIHRpbWVJbk9uZUhvdXIgOiBUYXNrIHggVGltZS5Qb3NpeFxuICAgIHRpbWVJbk9uZUhvdXIgPVxuICAgICAgICBQcm9jZXNzLnNsZWVwICg2MCAqIDYwICogMTAwMClcbiAgICAgICAgICAgIHw+IGFuZFRoZW4gKFxcXyAtPiBUaW1lLm5vdylcblxuRmlyc3QgdGhlIHByb2Nlc3Mgc2xlZXBzIGZvciBhbiBob3VyICoqYW5kIHRoZW4qKiBpdCB0ZWxscyB1cyB3aGF0IHRpbWUgaXQgaXMuXG5cbi19XG5hbmRUaGVuIDogKGEgLT4gVGFzayB4IGIpIC0+IFRhc2sgeCBhIC0+IFRhc2sgeCBiXG5hbmRUaGVuID1cbiAgICBHcmVuLktlcm5lbC5TY2hlZHVsZXIuYW5kVGhlblxuXG5cbnstfCBUaGlzIGlzIGxpa2UgW2FuZFRoZW5dKGFuZFRoZW4pIGJ1dCB0aGUgYXJndW1lbnRzIGFyZSByZXZlcnNlZC4gVGhlIGNhbGxiYWNrXG5pcyB0aGUgbGFzdCBhcmd1bWVudCwgaW5zdGVhZCBvZiB0aGUgZmlyc3QuIFRoaXMgbWFrZXMgaXQgZWFzaWVyIHRvIHdyaXRlIGltcGVyYXRpdmVcbmNvZGUgd2hlcmUgZWFjaCBjYWxsYmFjayBpbnZvbHZlcyBtb3JlIGxvZ2ljLlxuXG4gICAgaW1wb3J0IFByb2Nlc3NcbiAgICBpbXBvcnQgVGltZVxuXG4gICAgdGltZUluT25lSG91ciA6IFRhc2sgeCBUaW1lLlBvc2l4XG4gICAgdGltZUluT25lSG91ciA9XG4gICAgICAgIFRhc2suYXdhaXQgKFByb2Nlc3Muc2xlZXAgPHwgNjAgKiA2MCAqIDEwMDApIDx8IFxcXyAtPlxuICAgICAgICAgICAgVGltZS5ub3dcblxuKGEpd2FpdCBmb3IgYW4gaG91ciwgdGhlbiBmZXRjaCB0aGUgY3VycmVudCB0aW1lLlxuXG4tfVxuYXdhaXQgOiBUYXNrIHggYSAtPiAoYSAtPiBUYXNrIHggYikgLT4gVGFzayB4IGJcbmF3YWl0IHRzayBjYWxsYmFjayA9XG4gICAgR3Jlbi5LZXJuZWwuU2NoZWR1bGVyLmFuZFRoZW4gY2FsbGJhY2sgdHNrXG5cblxuLS0gRVJST1JTXG5cblxuey18IFJlY292ZXIgZnJvbSBhIGZhaWx1cmUgaW4gYSB0YXNrLiBJZiB0aGUgZ2l2ZW4gdGFzayBmYWlscywgd2UgdXNlIHRoZVxuY2FsbGJhY2sgdG8gcmVjb3Zlci5cblxuICAgIGZhaWwgXCJmaWxlIG5vdCBmb3VuZFwiXG4gICAgICB8PiBvbkVycm9yIChcXG1zZyAtPiBzdWNjZWVkIDQyKVxuICAgICAgLS0gc3VjY2VlZCA0MlxuXG4gICAgc3VjY2VlZCA5XG4gICAgICB8PiBvbkVycm9yIChcXG1zZyAtPiBzdWNjZWVkIDQyKVxuICAgICAgLS0gc3VjY2VlZCA5XG5cbi19XG5vbkVycm9yIDogKHggLT4gVGFzayB5IGEpIC0+IFRhc2sgeCBhIC0+IFRhc2sgeSBhXG5vbkVycm9yID1cbiAgICBHcmVuLktlcm5lbC5TY2hlZHVsZXIub25FcnJvclxuXG5cbnstfCBUcmFuc2Zvcm0gdGhlIGVycm9yIHZhbHVlLiBUaGlzIGNhbiBiZSB1c2VmdWwgaWYgeW91IG5lZWQgYSBidW5jaCBvZiBlcnJvclxudHlwZXMgdG8gbWF0Y2ggdXAuXG5cbiAgICB0eXBlIEVycm9yXG4gICAgICAgID0gSHR0cCBIdHRwLkVycm9yXG4gICAgICAgIHwgV2ViR0wgV2ViR0wuRXJyb3JcblxuICAgIGdldFJlc291cmNlcyA6IFRhc2sgRXJyb3IgUmVzb3VyY2VcbiAgICBnZXRSZXNvdXJjZXMgPVxuICAgICAgICBzZXF1ZW5jZVxuICAgICAgICAgICAgWyBtYXBFcnJvciBIdHRwIHNlcnZlclRhc2tcbiAgICAgICAgICAgICwgbWFwRXJyb3IgV2ViR0wgdGV4dHVyZVRhc2tcbiAgICAgICAgICAgIF1cblxuLX1cbm1hcEVycm9yIDogKHggLT4geSkgLT4gVGFzayB4IGEgLT4gVGFzayB5IGFcbm1hcEVycm9yIGNvbnZlcnQgdGFzayA9XG4gICAgdGFza1xuICAgICAgICB8PiBvbkVycm9yIChmYWlsIDw8IGNvbnZlcnQpXG5cblxuXG4tLSBDT01NQU5EU1xuXG5cbnR5cGUgTXlDbWQgbXNnXG4gICAgPSBQZXJmb3JtIChUYXNrIE5ldmVyIG1zZylcbiAgICB8IEV4ZWN1dGUgKFRhc2sgTmV2ZXIge30pXG5cblxuey18IExpa2UgSSB3YXMgc2F5aW5nIGluIHRoZSBbYFRhc2tgXSgjVGFzaykgZG9jdW1lbnRhdGlvbiwganVzdCBoYXZpbmcgYVxuYFRhc2tgIGRvZXMgbm90IG1lYW4gaXQgaXMgZG9uZS4gV2UgbXVzdCBjb21tYW5kIEdyZW4gdG8gYHBlcmZvcm1gIHRoZSB0YXNrOlxuXG5cblxuICAgIGltcG9ydCBUYXNrXG4gICAgaW1wb3J0IFRpbWVcblxuICAgIHR5cGUgTXNnXG4gICAgICAgID0gQ2xpY2tcbiAgICAgICAgfCBTZWFyY2ggU3RyaW5nXG4gICAgICAgIHwgTmV3VGltZSBUaW1lLlBvc2l4XG5cbiAgICBnZXROZXdUaW1lIDogQ21kIE1zZ1xuICAgIGdldE5ld1RpbWUgPVxuICAgICAgICBUYXNrLnBlcmZvcm0gTmV3VGltZSBUaW1lLm5vd1xuXG5TbyB3ZSBoYXZlIGNoYW5nZWQgYSB0YXNrIGxpa2UgXCJtYWtlIGRlbGljaW91cyBsYXNhZ25hXCIgaW50byBhIGNvbW1hbmQgbGlrZVxuXCJIZXkgR3JlbiwgbWFrZSBkZWxpY2lvdXMgbGFzYWduYSBhbmQgZ2l2ZSBpdCB0byBteSBgdXBkYXRlYCBmdW5jdGlvbiBhcyBhXG5gTXNnYCB2YWx1ZS5cIlxuXG4tfVxucGVyZm9ybSA6IChhIC0+IG1zZykgLT4gVGFzayBOZXZlciBhIC0+IENtZCBtc2dcbnBlcmZvcm0gdG9NZXNzYWdlIHRhc2sgPVxuICAgIGNvbW1hbmQgKFBlcmZvcm0gKG1hcCB0b01lc3NhZ2UgdGFzaykpXG5cblxuey18IFRoaXMgaXMgdmVyeSBzaW1pbGFyIHRvIFtgcGVyZm9ybWBdKCNwZXJmb3JtKSBleGNlcHQgaXQgY2FuIGhhbmRsZSBmYWlsdXJlcyFcblNvIHdlIGNvdWxkIF9hdHRlbXB0XyB0byBmb2N1cyBvbiBhIGNlcnRhaW4gRE9NIG5vZGUgbGlrZSB0aGlzOlxuXG4gICAgLS0gZ3JlbiBpbnN0YWxsIGdyZW4tbGFuZy9icm93c2VyXG5cblxuICAgIGltcG9ydCBCcm93c2VyLkRvbVxuICAgIGltcG9ydCBUYXNrXG5cbiAgICB0eXBlIE1zZ1xuICAgICAgICA9IENsaWNrXG4gICAgICAgIHwgU2VhcmNoIFN0cmluZ1xuICAgICAgICB8IEZvY3VzIChSZXN1bHQgQnJvd3Nlci5Eb21FcnJvciB7fSlcblxuICAgIGZvY3VzIDogQ21kIE1zZ1xuICAgIGZvY3VzID1cbiAgICAgICAgVGFzay5hdHRlbXB0IEZvY3VzIChCcm93c2VyLkRvbS5mb2N1cyBcIm15LWFwcC1zZWFyY2gtYm94XCIpXG5cblNvIHRoZSB0YXNrIGlzIFwiZm9jdXMgb24gdGhpcyBET00gbm9kZVwiIGFuZCB3ZSBhcmUgdHVybmluZyBpdCBpbnRvIHRoZSBjb21tYW5kXG5cIkhleSBHcmVuLCBhdHRlbXB0IHRvIGZvY3VzIG9uIHRoaXMgRE9NIG5vZGUgYW5kIGdpdmUgbWUgYSBgTXNnYCBhYm91dCB3aGV0aGVyXG55b3Ugc3VjY2VlZGVkIG9yIGZhaWxlZC5cIlxuXG4tfVxuYXR0ZW1wdCA6IChSZXN1bHQgeCBhIC0+IG1zZykgLT4gVGFzayB4IGEgLT4gQ21kIG1zZ1xuYXR0ZW1wdCByZXN1bHRUb01lc3NhZ2UgdGFzayA9XG4gICAgY29tbWFuZFxuICAgICAgICAoUGVyZm9ybVxuICAgICAgICAgICAgKHRhc2tcbiAgICAgICAgICAgICAgICB8PiBhbmRUaGVuIChzdWNjZWVkIDw8IHJlc3VsdFRvTWVzc2FnZSA8PCBPaylcbiAgICAgICAgICAgICAgICB8PiBvbkVycm9yIChzdWNjZWVkIDw8IHJlc3VsdFRvTWVzc2FnZSA8PCBFcnIpXG4gICAgICAgICAgICApXG4gICAgICAgIClcblxuXG57LXwgU29tZXRpbWVzIHdlIHdhbnQgdG8gZ2l2ZSBhIGNvbW1hbmQgd2l0aG91dCBiZWluZyB0b2xkIGhvdyBpdCB3ZW50LiBNYXliZSB3ZVxuYXJlIGxvZ2dpbmcgc29tZXRoaW5nIHRvIHRoZSBzY3JlZW4sIG9yIGNoYW5naW5nIHRoZSBzY3JvbGwgcG9zaXRpb24gb2YgdGhlIHdpbmRvdy5cbkluIGVpdGhlciBjYXNlLCB0aGVyZSdzIHJlYWxseSBub3RoaW5nIGZvciB1cyB0byBkbyBhZnRlcndhcmRzLiBJbiB0aG9zZSBjYXNlc1xud2UgY2FuIHVzZSBgZXhlY3V0ZWAuXG4tfVxuZXhlY3V0ZSA6IFRhc2sgTmV2ZXIgYSAtPiBDbWQgbXNnXG5leGVjdXRlIHRhc2sgPVxuICAgIGNvbW1hbmQgKEV4ZWN1dGUgKG1hcCAoXFxfIC0+IHt9KSB0YXNrKSlcblxuXG5jbWRNYXAgOiAoYSAtPiBiKSAtPiBNeUNtZCBhIC0+IE15Q21kIGJcbmNtZE1hcCB0YWdnZXIgY21kID1cbiAgICBjYXNlIGNtZCBvZlxuICAgICAgICBQZXJmb3JtIHRhc2sgLT5cbiAgICAgICAgICAgIFBlcmZvcm0gKG1hcCB0YWdnZXIgdGFzaylcblxuICAgICAgICBFeGVjdXRlIHRhc2sgLT5cbiAgICAgICAgICAgIEV4ZWN1dGUgdGFza1xuXG5cbi0tIE1BTkFHRVJcblxuXG5pbml0IDogVGFzayBOZXZlciB7fVxuaW5pdCA9XG4gICAgc3VjY2VlZCB7fVxuXG5cbm9uRWZmZWN0cyA6IFBsYXRmb3JtLlJvdXRlciBtc2cgTmV2ZXIgLT4gQXJyYXkgKE15Q21kIG1zZykgLT4ge30gLT4gVGFzayBOZXZlciB7fVxub25FZmZlY3RzIHJvdXRlciBjb21tYW5kcyBzdGF0ZSA9XG4gICAgbWFwXG4gICAgICAgIChcXF8gLT4ge30pXG4gICAgICAgIChzZXF1ZW5jZSAoQXJyYXkubWFwIChzcGF3bkNtZCByb3V0ZXIpIGNvbW1hbmRzKSlcblxuXG5vblNlbGZNc2cgOiBQbGF0Zm9ybS5Sb3V0ZXIgbXNnIE5ldmVyIC0+IE5ldmVyIC0+IHt9IC0+IFRhc2sgTmV2ZXIge31cbm9uU2VsZk1zZyBfIF8gXyA9XG4gICAgc3VjY2VlZCB7fVxuXG5cbnNwYXduQ21kIDogUGxhdGZvcm0uUm91dGVyIG1zZyBOZXZlciAtPiBNeUNtZCBtc2cgLT4gVGFzayB4IHt9XG5zcGF3bkNtZCByb3V0ZXIgY21kID1cbiAgICBjYXNlIGNtZCBvZlxuICAgICAgICBQZXJmb3JtIHRhc2sgLT5cbiAgICAgICAgICAgIEdyZW4uS2VybmVsLlNjaGVkdWxlci5zcGF3blxuICAgICAgICAgICAgICAgICh0YXNrXG4gICAgICAgICAgICAgICAgICAgIHw+IGFuZFRoZW4gKFBsYXRmb3JtLnNlbmRUb0FwcCByb3V0ZXIpXG4gICAgICAgICAgICAgICAgKVxuXG4gICAgICAgIEV4ZWN1dGUgdGFzayAtPlxuICAgICAgICAgICAgR3Jlbi5LZXJuZWwuU2NoZWR1bGVyLnNwYXduIHRhc2tcbiIsCiAgICAgICAgIm1vZHVsZSBCYXNpY3MgZXhwb3NpbmdcbiAgICAoIEludCwgKCspLCAoLSksICgqKSwgKC8pLCAoLy8pLCAoXiksIG5lZ2F0ZVxuICAgICwgRmxvYXQsIHRvRmxvYXQsIGlzTmFOLCBpc0luZmluaXRlXG4gICAgLCAoPT0pLCAoLz0pXG4gICAgLCAoPCksICg+KSwgKDw9KSwgKD49KSwgbWF4LCBtaW4sIGNsYW1wLCBjb21wYXJlLCBPcmRlciguLilcbiAgICAsIEJvb2woLi4pLCBub3QsICgmJiksICh8fCksIHhvclxuICAgICwgKCsrKVxuICAgICwgaWRlbnRpdHksICg8fCksICh8PiksICg8PCksICg+PiksIE5ldmVyLCBuZXZlclxuICAgIClcblxuey18IFRvbnMgb2YgdXNlZnVsIGZ1bmN0aW9ucyB0aGF0IGdldCBpbXBvcnRlZCBieSBkZWZhdWx0LlxuXG5cbiMjIE51bWJlcnNcblxuQGRvY3MgSW50LCAoKyksICgtKSwgKCopLCAoLyksICgvLyksICheKSwgbmVnYXRlXG5cblxuIyMgRmxvYXRcblxuQGRvY3MgRmxvYXQsIHRvRmxvYXQsIGlzTmFOLCBpc0luZmluaXRlXG5cblxuIyMgRXF1YWxpdHlcblxuQGRvY3MgKD09KSwgKC89KVxuXG5cbiMjIENvbXBhcmlzb25cblxuVGhlc2UgZnVuY3Rpb25zIG9ubHkgd29yayBvbiBgY29tcGFyYWJsZWAgdHlwZXMuIFRoaXMgaW5jbHVkZXMgbnVtYmVycyxcbmNoYXJhY3RlcnMsIHN0cmluZ3MgYW5kIGFycmF5cyBvZiBjb21wYXJhYmxlIHRoaW5ncy5cblxuQGRvY3MgT3JkZXIsICg8KSwgKD4pLCAoPD0pLCAoPj0pLCBtYXgsIG1pbiwgY2xhbXAsIGNvbXBhcmVcblxuXG4jIyBCb29sZWFuc1xuXG5AZG9jcyBCb29sLCBub3QsICgmJiksICh8fCksIHhvclxuXG5cbiMjIEFwcGVuZCBTdHJpbmdzIGFuZCBMaXN0c1xuXG5AZG9jcyAoKyspXG5cblxuIyMgRnVuY3Rpb24gSGVscGVyc1xuXG5AZG9jcyBpZGVudGl0eSwgKDx8KSwgKHw+KSwgKDw8KSwgKD4+KSwgTmV2ZXIsIG5ldmVyXG5cbi19XG5cbmltcG9ydCBHcmVuLktlcm5lbC5CYXNpY3NcbmltcG9ydCBHcmVuLktlcm5lbC5VdGlsc1xuXG5cblxuLS0gSU5GSVggT1BFUkFUT1JTXG5cblxuaW5maXggcmlnaHQgMCAoPHwpID0gYXBMXG5pbmZpeCBsZWZ0ICAwICh8PikgPSBhcFJcbmluZml4IHJpZ2h0IDIgKHx8KSA9IG9yXG5pbmZpeCByaWdodCAzICgmJikgPSBhbmRcbmluZml4IG5vbiAgIDQgKD09KSA9IGVxXG5pbmZpeCBub24gICA0ICgvPSkgPSBuZXFcbmluZml4IG5vbiAgIDQgKDwpID0gbHRcbmluZml4IG5vbiAgIDQgKD4pID0gZ3RcbmluZml4IG5vbiAgIDQgKDw9KSA9IGxlXG5pbmZpeCBub24gICA0ICg+PSkgPSBnZVxuaW5maXggcmlnaHQgNSAoKyspID0gYXBwZW5kXG5pbmZpeCBsZWZ0ICA2ICgrKSA9IGFkZFxuaW5maXggbGVmdCAgNiAoLSkgPSBzdWJcbmluZml4IGxlZnQgIDcgKCopID0gbXVsXG5pbmZpeCBsZWZ0ICA3ICgvKSA9IGZkaXZcbmluZml4IGxlZnQgIDcgKC8vKSA9IGlkaXZcbmluZml4IHJpZ2h0IDggKF4pID0gcG93XG5pbmZpeCBsZWZ0ICA5ICg8PCkgPSBjb21wb3NlTFxuaW5maXggcmlnaHQgOSAoPj4pID0gY29tcG9zZVJcblxuXG5cbi0tIE1BVEhFTUFUSUNTXG5cblxuey18IEFuIGBJbnRgIGlzIGEgd2hvbGUgbnVtYmVyLiBWYWxpZCBzeW50YXggZm9yIGludGVnZXJzIGluY2x1ZGVzOlxuXG4gICAgMFxuXG4gICAgNDJcblxuICAgIDkwMDBcblxuICAgIDB4RkYgLS0gMjU1IGluIGhleGFkZWNpbWFsXG5cbiAgICAweDBBIC0tICAxMCBpbiBoZXhhZGVjaW1hbFxuXG4qKk5vdGU6KiogYEludGAgbWF0aCBpcyB3ZWxsLWRlZmluZWQgaW4gdGhlIHJhbmdlIGAtMl4zMWAgdG8gYDJeMzEgLSAxYC4gT3V0c2lkZVxub2YgdGhhdCByYW5nZSwgdGhlIGJlaGF2aW9yIGlzIGRldGVybWluZWQgYnkgdGhlIGNvbXBpbGF0aW9uIHRhcmdldC4gV2hlblxuZ2VuZXJhdGluZyBKYXZhU2NyaXB0LCB0aGUgc2FmZSByYW5nZSBleHBhbmRzIHRvIGAtKDJeNTMgLSAxKWAgdG8gYDJeNTMgLSAxYCBmb3Igc29tZVxub3BlcmF0aW9ucywgYnV0IGlmIHdlIGdlbmVyYXRlIFdlYkFzc2VtYmx5IHNvbWUgZGF5LCB3ZSB3b3VsZCBkbyB0aGUgdHJhZGl0aW9uYWxcbltpbnRlZ2VyIG92ZXJmbG93XVtpb10uIFRoaXMgcXVpcmsgaXMgbmVjZXNzYXJ5IHRvIGdldCBnb29kIHBlcmZvcm1hbmNlIG9uXG5xdWlya3kgY29tcGlsYXRpb24gdGFyZ2V0cy5cblxuKipIaXN0b3JpY2FsIE5vdGU6KiogVGhlIG5hbWUgYEludGAgY29tZXMgZnJvbSB0aGUgdGVybSBbaW50ZWdlcl0uIEl0IGFwcGVhcnNcbnRoYXQgdGhlIGBpbnRgIGFiYnJldmlhdGlvbiB3YXMgaW50cm9kdWNlZCBpbiBbQUxHT0wgNjhdWzY4XSwgc2hvcnRlbmluZyBpdFxuZnJvbSBgaW50ZWdlcmAgaW4gW0FMR09MIDYwXVs2MF0uIFRvZGF5LCBhbG1vc3QgYWxsIHByb2dyYW1taW5nIGxhbmd1YWdlcyB1c2VcbnRoaXMgYWJicmV2aWF0aW9uLlxuXG5baW9dOiBodHRwczovL2VuLndpa2lwZWRpYS5vcmcvd2lraS9JbnRlZ2VyX292ZXJmbG93XG5baW50ZWdlcl06IGh0dHBzOi8vZW4ud2lraXBlZGlhLm9yZy93aWtpL0ludGVnZXJcbls2MF06IGh0dHBzOi8vZW4ud2lraXBlZGlhLm9yZy93aWtpL0FMR09MXzYwXG5bNjhdOiBodHRwczovL2VuLndpa2lwZWRpYS5vcmcvd2lraS9BTEdPTF82OFxuXG4tfVxudHlwZSBJbnRcbiAgICA9IEludCAtLSBOT1RFOiBUaGUgY29tcGlsZXIgcHJvdmlkZXMgdGhlIHJlYWwgaW1wbGVtZW50YXRpb24uXG5cblxuey18IEEgYEZsb2F0YCBpcyBhIFtmbG9hdGluZy1wb2ludCBudW1iZXJdW2ZwXS4gVmFsaWQgc3ludGF4IGZvciBmbG9hdHMgaW5jbHVkZXM6XG5cbiAgICAwXG4gICAgNDJcbiAgICAzLjE0XG4gICAgMC4xMjM0XG4gICAgNi4wMjJlMjMgICAtLSA9PSAoNi4wMjIgKiAxMF4yMylcbiAgICA2LjAyMmUrMjMgIC0tID09ICg2LjAyMiAqIDEwXjIzKVxuICAgIDEuNjAyZeKIkjE5ICAtLSA9PSAoMS42MDIgKiAxMF4tMTkpXG4gICAgMWUzICAgICAgICAtLSA9PSAoMSAqIDEwXjMpID09IDEwMDBcblxuKipIaXN0b3JpY2FsIE5vdGU6KiogVGhlIHBhcnRpY3VsYXIgZGV0YWlscyBvZiBmbG9hdHMgKGUuZy4gYE5hTmApIGFyZVxuc3BlY2lmaWVkIGJ5IFtJRUVFIDc1NF1baWVlZV0gd2hpY2ggaXMgbGl0ZXJhbGx5IGhhcmQtY29kZWQgaW50byBhbG1vc3QgYWxsXG5DUFVzIGluIHRoZSB3b3JsZC4gVGhhdCBtZWFucyBpZiB5b3UgdGhpbmsgYE5hTmAgaXMgd2VpcmQsIHlvdSBtdXN0XG5zdWNjZXNzZnVsbHkgb3ZlcnRha2UgSW50ZWwgYW5kIEFNRCB3aXRoIGEgY2hpcCB0aGF0IGlzIG5vdCBiYWNrd2FyZHNcbmNvbXBhdGlibGUgd2l0aCBhbnkgd2lkZWx5LXVzZWQgYXNzZW1ibHkgbGFuZ3VhZ2UuXG5cbltmcF06IGh0dHBzOi8vZW4ud2lraXBlZGlhLm9yZy93aWtpL0Zsb2F0aW5nLXBvaW50X2FyaXRobWV0aWNcbltpZWVlXTogaHR0cHM6Ly9lbi53aWtpcGVkaWEub3JnL3dpa2kvSUVFRV83NTRcblxuLX1cbnR5cGUgRmxvYXRcbiAgICA9IEZsb2F0IC0tIE5PVEU6IFRoZSBjb21waWxlciBwcm92aWRlcyB0aGUgcmVhbCBpbXBsZW1lbnRhdGlvbi5cblxuXG57LXwgQWRkIHR3byBudW1iZXJzLiBUaGUgYG51bWJlcmAgdHlwZSB2YXJpYWJsZSBtZWFucyB0aGlzIG9wZXJhdGlvbiBjYW4gYmVcbnNwZWNpYWxpemVkIHRvIGBJbnQgLT4gSW50IC0+IEludGAgb3IgdG8gYEZsb2F0IC0+IEZsb2F0IC0+IEZsb2F0YC4gU28geW91XG5jYW4gZG8gdGhpbmdzIGxpa2UgdGhpczpcblxuICAgIDMwMDIgKyA0MDA0ID09IDcwMDYgLS0gYWxsIGludHNcblxuICAgIDMuMTQgKyAzLjE0ID09IDYuMjggLS0gYWxsIGZsb2F0c1xuXG5Zb3UgX2Nhbm5vdF8gYWRkIGFuIGBJbnRgIGFuZCBhIGBGbG9hdGAgZGlyZWN0bHkgdGhvdWdoLiBVc2UgZnVuY3Rpb25zIGxpa2Vcblt0b0Zsb2F0XSgjdG9GbG9hdCkgb3IgW3JvdW5kXSgjcm91bmQpIHRvIGNvbnZlcnQgYm90aCB2YWx1ZXMgdG8gdGhlIHNhbWUgdHlwZS5cblNvIGlmIHlvdSBuZWVkZWQgdG8gYWRkIGEgbGlzdCBsZW5ndGggdG8gYSBgRmxvYXRgIGZvciBzb21lIHJlYXNvbiwgeW91XG5jb3VsZCBzYXkgb25lIG9mIHRoZXNlOlxuXG4gICAgMy4xNCArIHRvRmxvYXQgKExpc3QubGVuZ3RoIFsgMSwgMiwgMyBdKSA9PSA2LjE0XG5cbiAgICByb3VuZCAzLjE0ICsgTGlzdC5sZW5ndGggWyAxLCAyLCAzIF0gPT0gNlxuXG4qKk5vdGU6KiogTGFuZ3VhZ2VzIGxpa2UgSmF2YSBhbmQgSmF2YVNjcmlwdCBhdXRvbWF0aWNhbGx5IGNvbnZlcnQgYEludGAgdmFsdWVzXG50byBgRmxvYXRgIHZhbHVlcyB3aGVuIHlvdSBtaXggYW5kIG1hdGNoLiBUaGlzIGNhbiBtYWtlIGl0IGRpZmZpY3VsdCB0byBiZSBzdXJlXG5leGFjdGx5IHdoYXQgdHlwZSBvZiBudW1iZXIgeW91IGFyZSBkZWFsaW5nIHdpdGguIFdoZW4geW91IHRyeSB0byBfaW5mZXJfIHRoZXNlXG5jb252ZXJzaW9ucyAoYXMgU2NhbGEgZG9lcykgaXQgY2FuIGJlIGV2ZW4gbW9yZSBjb25mdXNpbmcuIEdyZW4gaGFzIG9wdGVkIGZvciBhXG5kZXNpZ24gdGhhdCBtYWtlcyBhbGwgY29udmVyc2lvbnMgZXhwbGljaXQuXG5cbi19XG5hZGQgOiBudW1iZXIgLT4gbnVtYmVyIC0+IG51bWJlclxuYWRkID1cbiAgICBHcmVuLktlcm5lbC5CYXNpY3MuYWRkXG5cblxuey18IFN1YnRyYWN0IG51bWJlcnMgbGlrZSBgNCAtIDMgPT0gMWAuXG5cblNlZSBbYCgrKWBdKCMrKSBmb3IgZG9jcyBvbiB0aGUgYG51bWJlcmAgdHlwZSB2YXJpYWJsZS5cblxuLX1cbnN1YiA6IG51bWJlciAtPiBudW1iZXIgLT4gbnVtYmVyXG5zdWIgPVxuICAgIEdyZW4uS2VybmVsLkJhc2ljcy5zdWJcblxuXG57LXwgTXVsdGlwbHkgbnVtYmVycyBsaWtlIGAyICogMyA9PSA2YC5cblxuU2VlIFtgKCspYF0oIyspIGZvciBkb2NzIG9uIHRoZSBgbnVtYmVyYCB0eXBlIHZhcmlhYmxlLlxuXG4tfVxubXVsIDogbnVtYmVyIC0+IG51bWJlciAtPiBudW1iZXJcbm11bCA9XG4gICAgR3Jlbi5LZXJuZWwuQmFzaWNzLm11bFxuXG5cbnstfCBGbG9hdGluZy1wb2ludCBkaXZpc2lvbjpcblxuICAgIDEwIC8gNCA9PSAyLjVcblxuICAgIDExIC8gNCA9PSAyLjc1XG5cbiAgICAxMiAvIDQgPT0gM1xuXG4gICAgMTMgLyA0ID09IDMuMjVcblxuICAgIDE0XG4gICAgICAgIC8gNFxuICAgICAgICA9PSAzLjVcbiAgICAgICAgLSAxXG4gICAgICAgIC8gNFxuICAgICAgICA9PSAtMC4yNVxuICAgICAgICAtIDVcbiAgICAgICAgLyA0XG4gICAgICAgID09IC0xLjI1XG5cbi19XG5mZGl2IDogRmxvYXQgLT4gRmxvYXQgLT4gRmxvYXRcbmZkaXYgPVxuICAgIEdyZW4uS2VybmVsLkJhc2ljcy5mZGl2XG5cblxuey18IEludGVnZXIgZGl2aXNpb246XG5cbiAgICAxMCAvLyA0ID09IDJcblxuICAgIDExIC8vIDQgPT0gMlxuXG4gICAgMTIgLy8gNCA9PSAzXG5cbiAgICAxMyAvLyA0ID09IDNcblxuICAgIDE0XG4gICAgICAgIC8vIDRcbiAgICAgICAgPT0gM1xuICAgICAgICAtIDFcbiAgICAgICAgLy8gNFxuICAgICAgICA9PSAwXG4gICAgICAgIC0gNVxuICAgICAgICAvLyA0XG4gICAgICAgID09IC0xXG5cbk5vdGljZSB0aGF0IHRoZSByZW1haW5kZXIgaXMgZGlzY2FyZGVkLCBzbyBgMyAvLyA0YCBpcyBnaXZpbmcgb3V0cHV0XG5zaW1pbGFyIHRvIGB0cnVuY2F0ZSAoMyAvIDQpYC5cblxuSXQgbWF5IHNvbWV0aW1lcyBiZSB1c2VmdWwgdG8gcGFpciB0aGlzIHdpdGggdGhlIFtgcmVtYWluZGVyQnlgXSgjcmVtYWluZGVyQnkpXG5mdW5jdGlvbi5cblxuLX1cbmlkaXYgOiBJbnQgLT4gSW50IC0+IEludFxuaWRpdiA9XG4gICAgR3Jlbi5LZXJuZWwuQmFzaWNzLmlkaXZcblxuXG57LXwgRXhwb25lbnRpYXRpb25cblxuICAgIDMgXiAyID09IDlcblxuICAgIDMgXiAzID09IDI3XG5cbi19XG5wb3cgOiBudW1iZXIgLT4gbnVtYmVyIC0+IG51bWJlclxucG93ID1cbiAgICBHcmVuLktlcm5lbC5CYXNpY3MucG93XG5cblxuey18IE5lZ2F0ZSBhIG51bWJlci5cblxuICAgIG5lZ2F0ZSA0MiA9PSAtNDJcblxuICAgIG5lZ2F0ZSAtNDIgPT0gNDJcblxuICAgIG5lZ2F0ZSAwID09IDBcblxuLX1cbm5lZ2F0ZSA6IG51bWJlciAtPiBudW1iZXJcbm5lZ2F0ZSBuID1cbiAgICAtblxuXG5cbi0tIElOVCBUTyBGTE9BVCAvIEZMT0FUIFRPIElOVFxuXG5cbnstfCBDb252ZXJ0IGFuIGludGVnZXIgaW50byBhIGZsb2F0LiBVc2VmdWwgd2hlbiBtaXhpbmcgYEludGAgYW5kIGBGbG9hdGBcbnZhbHVlcyBsaWtlIHRoaXM6XG5cbiAgICBoYWxmT2YgOiBJbnQgLT4gRmxvYXRcbiAgICBoYWxmT2YgbnVtYmVyID1cbiAgICAgICAgdG9GbG9hdCBudW1iZXIgLyAyXG5cbi19XG50b0Zsb2F0IDogSW50IC0+IEZsb2F0XG50b0Zsb2F0ID1cbiAgICBHcmVuLktlcm5lbC5CYXNpY3MudG9GbG9hdFxuXG5cblxuLS0gRVFVQUxJVFlcblxuXG57LXwgQ2hlY2sgaWYgdmFsdWVzIGFyZSAmbGRxdW87dGhlIHNhbWUmcmRxdW87LlxuXG4qKk5vdGU6KiogR3JlbiB1c2VzIHN0cnVjdHVyYWwgZXF1YWxpdHkgb24gdHVwbGVzLCByZWNvcmRzLCBhbmQgdXNlci1kZWZpbmVkXG51bmlvbiB0eXBlcy4gVGhpcyBtZWFucyB0aGUgdmFsdWVzIGAoMywgNClgIGFuZCBgKDMsIDQpYCBhcmUgZGVmaW5pdGVseSBlcXVhbC5cblRoaXMgaXMgbm90IHRydWUgaW4gbGFuZ3VhZ2VzIGxpa2UgSmF2YVNjcmlwdCB0aGF0IHVzZSByZWZlcmVuY2UgZXF1YWxpdHkgb25cbm9iamVjdHMuXG5cbioqTm90ZToqKiBEbyBub3QgdXNlIGAoPT0pYCB3aXRoIGZ1bmN0aW9ucywgSlNPTiB2YWx1ZXMgZnJvbSBgZ3Jlbi9qc29uYCwgb3JcbnJlZ3VsYXIgZXhwcmVzc2lvbnMgZnJvbSBgZ3Jlbi9yZWdleGAuIEl0IGRvZXMgbm90IHdvcmsuIEl0IHdpbGwgY3Jhc2ggaWZcbnBvc3NpYmxlLiBXaXRoIEpTT04gdmFsdWVzLCBkZWNvZGUgdG8gR3JlbiB2YWx1ZXMgYmVmb3JlIGRvaW5nIGFueSBlcXVhbGl0eVxuY2hlY2tzIVxuXG5XaHkgaXMgaXQgbGlrZSB0aGlzPyBFcXVhbGl0eSBpbiB0aGUgR3JlbiBzZW5zZSBjYW4gYmUgZGlmZmljdWx0IG9yIGltcG9zc2libGVcbnRvIGNvbXB1dGUuIFByb3ZpbmcgdGhhdCBmdW5jdGlvbnMgYXJlIHRoZSBzYW1lIGlzIFt1bmRlY2lkYWJsZV0sIGFuZCBKU09OXG52YWx1ZXMgY2FuIGNvbWUgaW4gdGhyb3VnaCBwb3J0cyBhbmQgaGF2ZSBmdW5jdGlvbnMsIGN5Y2xlcywgYW5kIG5ldyBKUyBkYXRhXG50eXBlcyB0aGF0IGludGVyYWN0IHdlaXJkbHkgd2l0aCBvdXIgZXF1YWxpdHkgaW1wbGVtZW50YXRpb24uIEluIGEgZnV0dXJlXG5yZWxlYXNlLCB0aGUgY29tcGlsZXIgd2lsbCBkZXRlY3Qgd2hlbiBgKD09KWAgaXMgdXNlZCB3aXRoIHByb2JsZW1hdGljIHR5cGVzXG5hbmQgcHJvdmlkZSBhIGhlbHBmdWwgZXJyb3IgbWVzc2FnZSBhdCBjb21waWxlIHRpbWUuIFRoaXMgd2lsbCByZXF1aXJlIHNvbWVcbnByZXR0eSBzZXJpb3VzIGluZnJhc3RydWN0dXJlIHdvcmssIHNvIHRoZSBzdG9wZ2FwIGlzIHRvIGNyYXNoIGFzIHF1aWNrbHkgYXNcbnBvc3NpYmxlLlxuXG5bdW5kZWNpZGFibGVdOiBodHRwczovL2VuLndpa2lwZWRpYS5vcmcvd2lraS9VbmRlY2lkYWJsZV9wcm9ibGVtXG5cbi19XG5lcSA6IGEgLT4gYSAtPiBCb29sXG5lcSA9XG4gICAgR3Jlbi5LZXJuZWwuVXRpbHMuZXF1YWxcblxuXG57LXwgQ2hlY2sgaWYgdmFsdWVzIGFyZSBub3QgJmxkcXVvO3RoZSBzYW1lJnJkcXVvOy5cblxuU28gYChhIC89IGIpYCBpcyB0aGUgc2FtZSBhcyBgKG5vdCAoYSA9PSBiKSlgLlxuXG4tfVxubmVxIDogYSAtPiBhIC0+IEJvb2xcbm5lcSA9XG4gICAgR3Jlbi5LZXJuZWwuVXRpbHMubm90RXF1YWxcblxuXG5cbi0tIENPTVBBUklTT05TXG5cblxuey18IC19XG5sdCA6IGNvbXBhcmFibGUgLT4gY29tcGFyYWJsZSAtPiBCb29sXG5sdCA9XG4gICAgR3Jlbi5LZXJuZWwuVXRpbHMubHRcblxuXG57LXwgLX1cbmd0IDogY29tcGFyYWJsZSAtPiBjb21wYXJhYmxlIC0+IEJvb2xcbmd0ID1cbiAgICBHcmVuLktlcm5lbC5VdGlscy5ndFxuXG5cbnstfCAtfVxubGUgOiBjb21wYXJhYmxlIC0+IGNvbXBhcmFibGUgLT4gQm9vbFxubGUgPVxuICAgIEdyZW4uS2VybmVsLlV0aWxzLmxlXG5cblxuey18IC19XG5nZSA6IGNvbXBhcmFibGUgLT4gY29tcGFyYWJsZSAtPiBCb29sXG5nZSA9XG4gICAgR3Jlbi5LZXJuZWwuVXRpbHMuZ2VcblxuXG57LXwgRmluZCB0aGUgc21hbGxlciBvZiB0d28gY29tcGFyYWJsZXMuXG5cbiAgICBtaW4gNDIgMTIzNDU2NzggPT0gNDJcblxuICAgIG1pbiBcImFiY1wiIFwieHl6XCIgPT0gXCJhYmNcIlxuXG4tfVxubWluIDogY29tcGFyYWJsZSAtPiBjb21wYXJhYmxlIC0+IGNvbXBhcmFibGVcbm1pbiB4IHkgPVxuICAgIGlmIGx0IHggeSB0aGVuXG4gICAgICAgIHhcblxuICAgIGVsc2VcbiAgICAgICAgeVxuXG5cbnstfCBGaW5kIHRoZSBsYXJnZXIgb2YgdHdvIGNvbXBhcmFibGVzLlxuXG4gICAgbWF4IDQyIDEyMzQ1Njc4ID09IDEyMzQ1Njc4XG5cbiAgICBtYXggXCJhYmNcIiBcInh5elwiID09IFwieHl6XCJcblxuLX1cbm1heCA6IGNvbXBhcmFibGUgLT4gY29tcGFyYWJsZSAtPiBjb21wYXJhYmxlXG5tYXggeCB5ID1cbiAgICBpZiBndCB4IHkgdGhlblxuICAgICAgICB4XG5cbiAgICBlbHNlXG4gICAgICAgIHlcblxuXG57LXwgQ2xhbXBzIGEgbnVtYmVyIHdpdGhpbiBhIGdpdmVuIHJhbmdlLiBXaXRoIHRoZSBleHByZXNzaW9uXG5gY2xhbXAgMTAwIDIwMCB4YCB0aGUgcmVzdWx0cyBhcmUgYXMgZm9sbG93czpcblxuICAgIDEwMCAgICAgaWYgeCA8IDEwMFxuICAgICB4ICAgICAgaWYgMTAwIDw9IHggPCAyMDBcbiAgICAyMDAgICAgIGlmIDIwMCA8PSB4XG5cbi19XG5jbGFtcCA6IG51bWJlciAtPiBudW1iZXIgLT4gbnVtYmVyIC0+IG51bWJlclxuY2xhbXAgbG93IGhpZ2ggbnVtYmVyID1cbiAgICBpZiBsdCBudW1iZXIgbG93IHRoZW5cbiAgICAgICAgbG93XG5cbiAgICBlbHNlIGlmIGd0IG51bWJlciBoaWdoIHRoZW5cbiAgICAgICAgaGlnaFxuXG4gICAgZWxzZVxuICAgICAgICBudW1iZXJcblxuXG57LXwgQ29tcGFyZSBhbnkgdHdvIGNvbXBhcmFibGUgdmFsdWVzLiBDb21wYXJhYmxlIHZhbHVlcyBpbmNsdWRlIGBTdHJpbmdgLFxuYENoYXJgLCBgSW50YCwgYEZsb2F0YCwgb3IgYSBsaXN0IG9yIHR1cGxlIGNvbnRhaW5pbmcgY29tcGFyYWJsZSB2YWx1ZXMuIFRoZXNlXG5hcmUgYWxzbyB0aGUgb25seSB2YWx1ZXMgdGhhdCB3b3JrIGFzIGBEaWN0YCBrZXlzIG9yIGBTZXRgIG1lbWJlcnMuXG5cbiAgICBjb21wYXJlIDMgNCA9PSBMVFxuXG4gICAgY29tcGFyZSA0IDQgPT0gRVFcblxuICAgIGNvbXBhcmUgNSA0ID09IEdUXG5cbi19XG5jb21wYXJlIDogY29tcGFyYWJsZSAtPiBjb21wYXJhYmxlIC0+IE9yZGVyXG5jb21wYXJlID1cbiAgICBHcmVuLktlcm5lbC5VdGlscy5jb21wYXJlXG5cblxuey18IFJlcHJlc2VudHMgdGhlIHJlbGF0aXZlIG9yZGVyaW5nIG9mIHR3byB0aGluZ3MuXG5UaGUgcmVsYXRpb25zIGFyZSBsZXNzIHRoYW4sIGVxdWFsIHRvLCBhbmQgZ3JlYXRlciB0aGFuLlxuLX1cbnR5cGUgT3JkZXJcbiAgICA9IExUXG4gICAgfCBFUVxuICAgIHwgR1RcblxuXG5cbi0tIEJPT0xFQU5TXG5cblxuey18IEEg4oCcQm9vbGVhbuKAnSB2YWx1ZS4gSXQgY2FuIGVpdGhlciBiZSBgVHJ1ZWAgb3IgYEZhbHNlYC5cblxuKipOb3RlOioqIFByb2dyYW1tZXJzIGNvbWluZyBmcm9tIEphdmFTY3JpcHQsIEphdmEsIGV0Yy4gdGVuZCB0byByZWFjaCBmb3JcbmJvb2xlYW4gdmFsdWVzIHdheSB0b28gb2Z0ZW4gaW4gR3Jlbi4gVXNpbmcgYSBbdW5pb24gdHlwZV1bdXRdIGlzIG9mdGVuIGNsZWFyZXJcbmFuZCBtb3JlIHJlbGlhYmxlLiBZb3UgY2FuIGxlYXJuIG1vcmUgYWJvdXQgdGhpcyBmcm9tIEplcmVteSBbaGVyZV1bamZdIG9yXG5mcm9tIFJpY2hhcmQgW2hlcmVdW3J0XS5cblxuW3V0XTogaHR0cHM6Ly9ndWlkZS5ncmVuLWxhbmcub3JnL3R5cGVzL3VuaW9uX3R5cGVzLmh0bWxcbltqZl06IGh0dHBzOi8veW91dHUuYmUvNlRES0hHdEF4ZWc/dD0xbTI1c1xuW3J0XTogaHR0cHM6Ly95b3V0dS5iZS9JY2dtU1JKSHVfOD90PTFtMTRzXG5cbi19XG50eXBlIEJvb2xcbiAgICA9IFRydWVcbiAgICB8IEZhbHNlXG5cblxuey18IE5lZ2F0ZSBhIGJvb2xlYW4gdmFsdWUuXG5cbiAgICBub3QgVHJ1ZSA9PSBGYWxzZVxuXG4gICAgbm90IEZhbHNlID09IFRydWVcblxuLX1cbm5vdCA6IEJvb2wgLT4gQm9vbFxubm90ID1cbiAgICBHcmVuLktlcm5lbC5CYXNpY3Mubm90XG5cblxuey18IFRoZSBsb2dpY2FsIEFORCBvcGVyYXRvci4gYFRydWVgIGlmIGJvdGggaW5wdXRzIGFyZSBgVHJ1ZWAuXG5cbiAgICBUcnVlICYmIFRydWUgPT0gVHJ1ZVxuXG4gICAgVHJ1ZSAmJiBGYWxzZSA9PSBGYWxzZVxuXG4gICAgRmFsc2UgJiYgVHJ1ZSA9PSBGYWxzZVxuXG4gICAgRmFsc2UgJiYgRmFsc2UgPT0gRmFsc2VcblxuKipOb3RlOioqIFdoZW4gdXNlZCBpbiB0aGUgaW5maXggcG9zaXRpb24sIGxpa2UgYChsZWZ0ICYmIHJpZ2h0KWAsIHRoZSBvcGVyYXRvclxuc2hvcnQtY2lyY3VpdHMuIFRoaXMgbWVhbnMgaWYgYGxlZnRgIGlzIGBGYWxzZWAgd2UgZG8gbm90IGJvdGhlciBldmFsdWF0aW5nIGByaWdodGBcbmFuZCBqdXN0IHJldHVybiBgRmFsc2VgIG92ZXJhbGwuXG5cbi19XG5hbmQgOiBCb29sIC0+IEJvb2wgLT4gQm9vbFxuYW5kID1cbiAgICBHcmVuLktlcm5lbC5CYXNpY3MuYW5kXG5cblxuey18IFRoZSBsb2dpY2FsIE9SIG9wZXJhdG9yLiBgVHJ1ZWAgaWYgb25lIG9yIGJvdGggaW5wdXRzIGFyZSBgVHJ1ZWAuXG5cbiAgICBUcnVlIHx8IFRydWUgPT0gVHJ1ZVxuXG4gICAgVHJ1ZSB8fCBGYWxzZSA9PSBUcnVlXG5cbiAgICBGYWxzZSB8fCBUcnVlID09IFRydWVcblxuICAgIEZhbHNlIHx8IEZhbHNlID09IEZhbHNlXG5cbioqTm90ZToqKiBXaGVuIHVzZWQgaW4gdGhlIGluZml4IHBvc2l0aW9uLCBsaWtlIGAobGVmdCB8fCByaWdodClgLCB0aGUgb3BlcmF0b3JcbnNob3J0LWNpcmN1aXRzLiBUaGlzIG1lYW5zIGlmIGBsZWZ0YCBpcyBgVHJ1ZWAgd2UgZG8gbm90IGJvdGhlciBldmFsdWF0aW5nIGByaWdodGBcbmFuZCBqdXN0IHJldHVybiBgVHJ1ZWAgb3ZlcmFsbC5cblxuLX1cbm9yIDogQm9vbCAtPiBCb29sIC0+IEJvb2xcbm9yID1cbiAgICBHcmVuLktlcm5lbC5CYXNpY3Mub3JcblxuXG57LXwgVGhlIGV4Y2x1c2l2ZS1vciBvcGVyYXRvci4gYFRydWVgIGlmIGV4YWN0bHkgb25lIGlucHV0IGlzIGBUcnVlYC5cblxuICAgIHhvciBUcnVlIFRydWUgPT0gRmFsc2VcblxuICAgIHhvciBUcnVlIEZhbHNlID09IFRydWVcblxuICAgIHhvciBGYWxzZSBUcnVlID09IFRydWVcblxuICAgIHhvciBGYWxzZSBGYWxzZSA9PSBGYWxzZVxuXG4tfVxueG9yIDogQm9vbCAtPiBCb29sIC0+IEJvb2xcbnhvciA9XG4gICAgR3Jlbi5LZXJuZWwuQmFzaWNzLnhvclxuXG5cblxuLS0gQVBQRU5EXG5cblxuey18IFB1dCB0d28gYXBwZW5kYWJsZSB0aGluZ3MgdG9nZXRoZXIuIFRoaXMgaW5jbHVkZXMgc3RyaW5ncyBhbmQgbGlzdHMuXG5cbiAgICBcImhlbGxvXCIgKysgXCJ3b3JsZFwiID09IFwiaGVsbG93b3JsZFwiXG5cbiAgICBbIDEsIDEsIDIgXSArKyBbIDMsIDUsIDggXSA9PSBbIDEsIDEsIDIsIDMsIDUsIDggXVxuXG4tfVxuYXBwZW5kIDogYXBwZW5kYWJsZSAtPiBhcHBlbmRhYmxlIC0+IGFwcGVuZGFibGVcbmFwcGVuZCA9XG4gICAgR3Jlbi5LZXJuZWwuVXRpbHMuYXBwZW5kXG5cblxuXG4tLSBDUkFaWSBGTE9BVFNcblxuXG57LXwgRGV0ZXJtaW5lIHdoZXRoZXIgYSBmbG9hdCBpcyBhbiB1bmRlZmluZWQgb3IgdW5yZXByZXNlbnRhYmxlIG51bWJlci5cbk5hTiBzdGFuZHMgZm9yIF9ub3QgYSBudW1iZXJfIGFuZCBpdCBpcyBbYSBzdGFuZGFyZGl6ZWQgcGFydCBvZiBmbG9hdGluZyBwb2ludFxubnVtYmVyc10oaHR0cHM6Ly9lbi53aWtpcGVkaWEub3JnL3dpa2kvTmFOKS5cblxuICAgIGlzTmFOICgwIC8gMCkgPT0gVHJ1ZVxuXG4gICAgaXNOYU4gKHNxcnQgLTEpID09IFRydWVcblxuICAgIGlzTmFOICgxIC8gMCkgPT0gRmFsc2UgLS0gaW5maW5pdHkgaXMgYSBudW1iZXJcblxuICAgIGlzTmFOIDEgPT0gRmFsc2VcblxuLX1cbmlzTmFOIDogRmxvYXQgLT4gQm9vbFxuaXNOYU4gPVxuICAgIEdyZW4uS2VybmVsLkJhc2ljcy5pc05hTlxuXG5cbnstfCBEZXRlcm1pbmUgd2hldGhlciBhIGZsb2F0IGlzIHBvc2l0aXZlIG9yIG5lZ2F0aXZlIGluZmluaXR5LlxuXG4gICAgaXNJbmZpbml0ZSAoMCAvIDApID09IEZhbHNlXG5cbiAgICBpc0luZmluaXRlIChzcXJ0IC0xKSA9PSBGYWxzZVxuXG4gICAgaXNJbmZpbml0ZSAoMSAvIDApID09IFRydWVcblxuICAgIGlzSW5maW5pdGUgMSA9PSBGYWxzZVxuXG5Ob3RpY2UgdGhhdCBOYU4gaXMgbm90IGluZmluaXRlISBGb3IgZmxvYXQgYG5gIHRvIGJlIGZpbml0ZSBpbXBsaWVzIHRoYXRcbmBub3QgKGlzSW5maW5pdGUgbiB8fCBpc05hTiBuKWAgZXZhbHVhdGVzIHRvIGBUcnVlYC5cblxuLX1cbmlzSW5maW5pdGUgOiBGbG9hdCAtPiBCb29sXG5pc0luZmluaXRlID1cbiAgICBHcmVuLktlcm5lbC5CYXNpY3MuaXNJbmZpbml0ZVxuXG5cblxuLS0gRlVOQ1RJT04gSEVMUEVSU1xuXG5cbnstfCBGdW5jdGlvbiBjb21wb3NpdGlvbiwgcGFzc2luZyByZXN1bHRzIGFsb25nIGluIHRoZSBzdWdnZXN0ZWQgZGlyZWN0aW9uLiBGb3JcbmV4YW1wbGUsIHRoZSBmb2xsb3dpbmcgY29kZSBjaGVja3MgaWYgdGhlIHJlc3VsdCBvZiByb3VuZGluZyBhIGZsb2F0IGlzIG9kZDpcblxuICAgIG5vdCA8PCBpc0V2ZW4gPDwgcm91bmRcblxuWW91IGNhbiB0aGluayBvZiB0aGlzIG9wZXJhdG9yIGFzIGVxdWl2YWxlbnQgdG8gdGhlIGZvbGxvd2luZzpcblxuICAgIChnIDw8IGYpID09IChcXHggLT4gZyAoZiB4KSlcblxuU28gb3VyIGV4YW1wbGUgZXhwYW5kcyBvdXQgdG8gc29tZXRoaW5nIGxpa2UgdGhpczpcblxuICAgIFxcbiAtPiBub3QgKGlzRXZlbiAocm91bmQgbikpXG5cbi19XG5jb21wb3NlTCA6IChiIC0+IGMpIC0+IChhIC0+IGIpIC0+IChhIC0+IGMpXG5jb21wb3NlTCBnIGYgeCA9XG4gICAgZyAoZiB4KVxuXG5cbnstfCBGdW5jdGlvbiBjb21wb3NpdGlvbiwgcGFzc2luZyByZXN1bHRzIGFsb25nIGluIHRoZSBzdWdnZXN0ZWQgZGlyZWN0aW9uLiBGb3JcbmV4YW1wbGUsIHRoZSBmb2xsb3dpbmcgY29kZSBjaGVja3MgaWYgdGhlIHJlc3VsdCBvZiByb3VuZGluZyBhIGZsb2F0IGlzIG9kZDpcblxuICAgIHJvdW5kID4+IGlzRXZlbiA+PiBub3RcblxuLX1cbmNvbXBvc2VSIDogKGEgLT4gYikgLT4gKGIgLT4gYykgLT4gKGEgLT4gYylcbmNvbXBvc2VSIGYgZyB4ID1cbiAgICBnIChmIHgpXG5cblxuey18IFNheWluZyBgeCB8PiBmYCBpcyBleGFjdGx5IHRoZSBzYW1lIGFzIGBmIHhgLlxuXG5JdCBpcyBjYWxsZWQgdGhlIOKAnHBpcGXigJ0gb3BlcmF0b3IgYmVjYXVzZSBpdCBsZXRzIHlvdSB3cml0ZSDigJxwaXBlbGluZWTigJ0gY29kZS5cbkZvciBleGFtcGxlLCBzYXkgd2UgaGF2ZSBhIGBzYW5pdGl6ZWAgZnVuY3Rpb24gZm9yIHR1cm5pbmcgdXNlciBpbnB1dCBpbnRvXG5pbnRlZ2VyczpcblxuICAgIC0tIEJFRk9SRVxuICAgIHNhbml0aXplIDogU3RyaW5nIC0+IE1heWJlIEludFxuICAgIHNhbml0aXplIGlucHV0ID1cbiAgICAgICAgU3RyaW5nLnRvSW50IChTdHJpbmcudHJpbSBpbnB1dClcblxuV2UgY2FuIHJld3JpdGUgaXQgbGlrZSB0aGlzOlxuXG4gICAgLS0gQUZURVJcbiAgICBzYW5pdGl6ZSA6IFN0cmluZyAtPiBNYXliZSBJbnRcbiAgICBzYW5pdGl6ZSBpbnB1dCA9XG4gICAgICAgIGlucHV0XG4gICAgICAgICAgICB8PiBTdHJpbmcudHJpbVxuICAgICAgICAgICAgfD4gU3RyaW5nLnRvSW50XG5cblRvdGFsbHkgZXF1aXZhbGVudCEgSSByZWNvbW1lbmQgdHJ5aW5nIHRvIHJld3JpdGUgY29kZSB0aGF0IHVzZXMgYHggfD4gZmBcbmludG8gY29kZSBsaWtlIGBmIHhgIHVudGlsIHRoZXJlIGFyZSBubyBwaXBlcyBsZWZ0LiBUaGF0IGNhbiBoZWxwIHlvdSBidWlsZFxueW91ciBpbnR1aXRpb24uXG5cbioqTm90ZToqKiBUaGlzIGNhbiBiZSBvdmVydXNlZCEgSSB0aGluayBmb2xrcyBmaW5kIGl0IHF1aXRlIG5lYXQsIGJ1dCB3aGVuIHlvdVxuaGF2ZSB0aHJlZSBvciBmb3VyIHN0ZXBzLCB0aGUgY29kZSBvZnRlbiBnZXRzIGNsZWFyZXIgaWYgeW91IGJyZWFrIG91dCBhXG50b3AtbGV2ZWwgaGVscGVyIGZ1bmN0aW9uLiBOb3cgdGhlIHRyYW5zZm9ybWF0aW9uIGhhcyBhIG5hbWUuIFRoZSBhcmd1bWVudHMgYXJlXG5uYW1lZC4gSXQgaGFzIGEgdHlwZSBhbm5vdGF0aW9uLiBJdCBpcyBtdWNoIG1vcmUgc2VsZi1kb2N1bWVudGluZyB0aGF0IHdheSFcblRlc3RpbmcgdGhlIGxvZ2ljIGdldHMgZWFzaWVyIHRvby4gTmljZSBzaWRlIGJlbmVmaXQhXG5cbi19XG5hcFIgOiBhIC0+IChhIC0+IGIpIC0+IGJcbmFwUiB4IGYgPVxuICAgIGYgeFxuXG5cbnstfCBTYXlpbmcgYGYgPHwgeGAgaXMgZXhhY3RseSB0aGUgc2FtZSBhcyBgZiB4YC5cblxuSXQgY2FuIGhlbHAgeW91IGF2b2lkIHBhcmVudGhlc2VzLCB3aGljaCBjYW4gYmUgbmljZSBzb21ldGltZXMuIE1heWJlIHlvdSB3YW50XG50byBhcHBseSBhIGZ1bmN0aW9uIHRvIGEgYGNhc2VgIGV4cHJlc3Npb24/IFRoYXQgc29ydCBvZiB0aGluZy5cblxuLX1cbmFwTCA6IChhIC0+IGIpIC0+IGEgLT4gYlxuYXBMIGYgeCA9XG4gICAgZiB4XG5cblxuey18IEdpdmVuIGEgdmFsdWUsIHJldHVybnMgZXhhY3RseSB0aGUgc2FtZSB2YWx1ZS4gVGhpcyBpcyBjYWxsZWRcblt0aGUgaWRlbnRpdHkgZnVuY3Rpb25dKGh0dHBzOi8vZW4ud2lraXBlZGlhLm9yZy93aWtpL0lkZW50aXR5X2Z1bmN0aW9uKS5cbi19XG5pZGVudGl0eSA6IGEgLT4gYVxuaWRlbnRpdHkgeCA9XG4gICAgeFxuXG5cbnstfCBBIHZhbHVlIHRoYXQgY2FuIG5ldmVyIGhhcHBlbiEgRm9yIGNvbnRleHQ6XG5cbiAgLSBUaGUgYm9vbGVhbiB0eXBlIGBCb29sYCBoYXMgdHdvIHZhbHVlczogYFRydWVgIGFuZCBgRmFsc2VgXG4gIC0gVGhlIHVuaXQgdHlwZSBgKClgIGhhcyBvbmUgdmFsdWU6IGAoKWBcbiAgLSBUaGUgbmV2ZXIgdHlwZSBgTmV2ZXJgIGhhcyBubyB2YWx1ZXMhXG5cbllvdSBtYXkgc2VlIGl0IGluIHRoZSB3aWxkIGluIGBIdG1sIE5ldmVyYCB3aGljaCBtZWFucyB0aGlzIEhUTUwgd2lsbCBuZXZlclxucHJvZHVjZSBhbnkgbWVzc2FnZXMuIFlvdSB3b3VsZCBuZWVkIHRvIHdyaXRlIGFuIGV2ZW50IGhhbmRsZXIgbGlrZVxuYG9uQ2xpY2sgPz8/IDogQXR0cmlidXRlIE5ldmVyYCBidXQgaG93IGNhbiB3ZSBmaWxsIGluIHRoZSBxdWVzdGlvbiBtYXJrcz8hXG5TbyB0aGVyZSBjYW5ub3QgYmUgYW55IGV2ZW50IGhhbmRsZXJzIG9uIHRoYXQgSFRNTC5cblxuWW91IG1heSBhbHNvIHNlZSB0aGlzIHVzZWQgd2l0aCB0YXNrcyB0aGF0IG5ldmVyIGZhaWwsIGxpa2UgYFRhc2sgTmV2ZXIgKClgLlxuXG5UaGUgYE5ldmVyYCB0eXBlIGlzIHVzZWZ1bCBmb3IgcmVzdHJpY3RpbmcgX2FyZ3VtZW50c18gdG8gYSBmdW5jdGlvbi4gTWF5YmUgbXlcbkFQSSBjYW4gb25seSBhY2NlcHQgSFRNTCB3aXRob3V0IGV2ZW50IGhhbmRsZXJzLCBzbyBJIHJlcXVpcmUgYEh0bWwgTmV2ZXJgIGFuZFxudXNlcnMgY2FuIGdpdmUgYEh0bWwgbXNnYCBhbmQgZXZlcnl0aGluZyB3aWxsIGdvIGZpbmUuIEdlbmVyYWxseSBzcGVha2luZywgeW91XG5kbyBub3Qgd2FudCBgTmV2ZXJgIGluIHlvdXIgcmV0dXJuIHR5cGVzIHRob3VnaC5cblxuLX1cbnR5cGUgTmV2ZXJcbiAgICA9IEp1c3RPbmVNb3JlIE5ldmVyXG5cblxuey18IEEgZnVuY3Rpb24gdGhhdCBjYW4gbmV2ZXIgYmUgY2FsbGVkLiBTZWVtcyBleHRyZW1lbHkgcG9pbnRsZXNzLCBidXQgaXRcbl9jYW5fIGNvbWUgaW4gaGFuZHkuIEltYWdpbmUgeW91IGhhdmUgc29tZSBIVE1MIHRoYXQgc2hvdWxkIG5ldmVyIHByb2R1Y2UgYW55XG5tZXNzYWdlcy4gQW5kIHNheSB5b3Ugd2FudCB0byB1c2UgaXQgaW4gc29tZSBvdGhlciBIVE1MIHRoYXQgX2RvZXNfIHByb2R1Y2Vcbm1lc3NhZ2VzLiBZb3UgY291bGQgc2F5OlxuXG4gICAgaW1wb3J0IEh0bWwgZXhwb3NpbmcgKC4uKVxuXG4gICAgZW1iZWRIdG1sIDogSHRtbCBOZXZlciAtPiBIdG1sIG1zZ1xuICAgIGVtYmVkSHRtbCBzdGF0aWNTdHVmZiA9XG4gICAgICAgIGRpdiBbXVxuICAgICAgICAgICAgWyB0ZXh0IFwiaGVsbG9cIlxuICAgICAgICAgICAgLCBIdG1sLm1hcCBuZXZlciBzdGF0aWNTdHVmZlxuICAgICAgICAgICAgXVxuXG5TbyB0aGUgYG5ldmVyYCBmdW5jdGlvbiBpcyBiYXNpY2FsbHkgdGVsbGluZyB0aGUgdHlwZSBzeXN0ZW0sIG1ha2Ugc3VyZSBubyBvbmVcbmV2ZXIgY2FsbHMgbWUhXG5cbi19XG5uZXZlciA6IE5ldmVyIC0+IGFcbm5ldmVyIChKdXN0T25lTW9yZSBudnIpID1cbiAgICBuZXZlciBudnJcbiIsCiAgICAgICAgIm1vZHVsZSBEaWN0IGV4cG9zaW5nXG4gICAgKCBEaWN0XG4gICAgLCBlbXB0eSwgc2luZ2xldG9uLCBzZXQsIHVwZGF0ZSwgcmVtb3ZlXG4gICAgLCBpc0VtcHR5LCBjb3VudCwgZ2V0LCBtZW1iZXIsIGZpcnN0LCBsYXN0LCBmaW5kRmlyc3QsIGZpbmRMYXN0LCBhbnksIGFsbFxuICAgICwga2V5cywgdmFsdWVzXG4gICAgLCBtYXAsIGZvbGRsLCBmb2xkciwgZmlsdGVyLCBmaWx0ZXJNYXAsIHBhcnRpdGlvblxuICAgICwgdW5pb24sIGludGVyc2VjdCwgZGlmZiwgbWVyZ2VcbiAgICApXG5cbnstfCBBIGRpY3Rpb25hcnkgbWFwcGluZyB1bmlxdWUga2V5cyB0byB2YWx1ZXMuIFRoZSBrZXlzIGNhbiBiZSBhbnkgY29tcGFyYWJsZVxudHlwZS4gVGhpcyBpbmNsdWRlcyBgSW50YCwgYEZsb2F0YCwgYFRpbWVgLCBgQ2hhcmAgYW5kIGBTdHJpbmdgLlxuXG5TZXQsIHJlbW92ZSwgYW5kIHF1ZXJ5IG9wZXJhdGlvbnMgYWxsIHRha2UgX08obG9nIG4pXyB0aW1lLlxuXG5cbkBkb2NzIERpY3RcblxuXG5AZG9jcyBlbXB0eSwgc2luZ2xldG9uLCBzZXQsIHVwZGF0ZSwgcmVtb3ZlXG5cblxuIyMgUXVlcnlcblxuQGRvY3MgaXNFbXB0eSwgY291bnQsIGdldCwgbWVtYmVyLCBmaXJzdCwgbGFzdCwgZmluZEZpcnN0LCBmaW5kTGFzdCwgYW55LCBhbGxcblxuXG4jIyBBcnJheXNcblxuQGRvY3Mga2V5cywgdmFsdWVzXG5cblxuIyMgVHJhbnNmb3JtXG5cbkBkb2NzIG1hcCwgZm9sZGwsIGZvbGRyLCBmaWx0ZXIsIGZpbHRlck1hcCwgcGFydGl0aW9uXG5cblxuIyMgQ29tYmluZVxuXG5AZG9jcyB1bmlvbiwgaW50ZXJzZWN0LCBkaWZmLCBtZXJnZVxuXG4tfVxuXG5cbmltcG9ydCBBcnJheSBleHBvc2luZyAoQXJyYXkpXG5pbXBvcnQgQmFzaWNzIGV4cG9zaW5nICguLilcbmltcG9ydCBNYXliZSBleHBvc2luZyAoLi4pXG5cblxuXG4tLSBESUNUSU9OQVJJRVNcbi0tIFRoZSBjb2xvciBvZiBhIG5vZGUuIExlYXZlcyBhcmUgY29uc2lkZXJlZCBCbGFjay5cblxuXG50eXBlIE5Db2xvclxuICAgID0gUmVkXG4gICAgfCBCbGFja1xuXG5cbnstfCBBIGRpY3Rpb25hcnkgb2Yga2V5cyBhbmQgdmFsdWVzLiBTbyBhIGBEaWN0IFN0cmluZyBVc2VyYCBpcyBhIGRpY3Rpb25hcnlcbnRoYXQgbGV0cyB5b3UgbG9vayB1cCBhIGBTdHJpbmdgIChzdWNoIGFzIHVzZXIgbmFtZXMpIGFuZCBmaW5kIHRoZSBhc3NvY2lhdGVkXG5gVXNlcmAuXG5cbiAgICBpbXBvcnQgRGljdCBleHBvc2luZyAoIERpY3QgKVxuXG4gICAgdXNlcnMgOiBEaWN0IFN0cmluZyBVc2VyXG4gICAgdXNlcnMgPVxuICAgICAgICBEaWN0LmZyb21BcnJheVxuICAgICAgICAgICAgWyB7IGtleSA9IFwiQWxpY2VcIlxuICAgICAgICAgICAgICAsIHZhbHVlID0gbWFrZVVzZXIgXCJBbGljZVwiIDI4IDEuNjVcbiAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgLCB7IGtleSA9IFwiQm9iXCJcbiAgICAgICAgICAgICAgLCB2YWx1ZSA9IG1ha2VVc2VyIFwiQm9iXCIgMTkgMS44MlxuICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAsIHsga2V5ID0gXCJDaHVja1wiXG4gICAgICAgICAgICAgICwgdmFsdWUgPSBtYWtlVXNlciBcIkNodWNrXCIgMzMgMS43NVxuICAgICAgICAgICAgICB9XG4gICAgICAgICAgICBdXG5cbiAgICB0eXBlIGFsaWFzIFVzZXIgPVxuICAgICAgICB7IG5hbWUgOiBTdHJpbmdcbiAgICAgICAgLCBhZ2UgOiBJbnRcbiAgICAgICAgLCBoZWlnaHQgOiBGbG9hdFxuICAgICAgICB9XG5cbiAgICBtYWtlVXNlciA6IFN0cmluZyAtPiBJbnQgLT4gRmxvYXQgLT4gVXNlclxuICAgIG1ha2VVc2VyIG5hbWUgYWdlIGhlaWdodCA9XG4gICAgICAgIHsgbmFtZSA9IG5hbWVcbiAgICAgICAgLCBhZ2UgPSBhZ2VcbiAgICAgICAgLCBoZWlnaHQgPSBoZWlnaHRcbiAgICAgICAgfVxuLX1cbnR5cGUgRGljdCBrIHZcbiAgICA9IFJCTm9kZV9ncmVuX2J1aWx0aW4gTkNvbG9yIGsgdiAoRGljdCBrIHYpIChEaWN0IGsgdilcbiAgICB8IFJCRW1wdHlfZ3Jlbl9idWlsdGluXG5cblxuey18IENyZWF0ZSBhbiBlbXB0eSBkaWN0aW9uYXJ5LlxuLX1cbmVtcHR5IDogRGljdCBrIHZcbmVtcHR5ID1cbiAgICBSQkVtcHR5X2dyZW5fYnVpbHRpblxuXG5cbnstfCBHZXQgdGhlIHZhbHVlIGFzc29jaWF0ZWQgd2l0aCBhIGtleS4gSWYgdGhlIGtleSBpcyBub3QgZm91bmQsIHJldHVyblxuYE5vdGhpbmdgLiBUaGlzIGlzIHVzZWZ1bCB3aGVuIHlvdSBhcmUgbm90IHN1cmUgaWYgYSBrZXkgd2lsbCBiZSBpbiB0aGVcbmRpY3Rpb25hcnkuXG5cbiAgICBhbmltYWxzID0gZnJvbUFycmF5IFsgKFwiVG9tXCIsIENhdCksIChcIkplcnJ5XCIsIE1vdXNlKSBdXG5cbiAgICBnZXQgXCJUb21cIiAgIGFuaW1hbHMgPT0gSnVzdCBDYXRcbiAgICBnZXQgXCJKZXJyeVwiIGFuaW1hbHMgPT0gSnVzdCBNb3VzZVxuICAgIGdldCBcIlNwaWtlXCIgYW5pbWFscyA9PSBOb3RoaW5nXG5cbi19XG5nZXQgOiBjb21wYXJhYmxlIC0+IERpY3QgY29tcGFyYWJsZSB2IC0+IE1heWJlIHZcbmdldCB0YXJnZXRLZXkgZGljdCA9XG4gICAgY2FzZSBkaWN0IG9mXG4gICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluIC0+XG4gICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBfIGtleSB2YWx1ZSBsZWZ0IHJpZ2h0IC0+XG4gICAgICAgICAgICBjYXNlIGNvbXBhcmUgdGFyZ2V0S2V5IGtleSBvZlxuICAgICAgICAgICAgICAgIExUIC0+XG4gICAgICAgICAgICAgICAgICAgIGdldCB0YXJnZXRLZXkgbGVmdFxuXG4gICAgICAgICAgICAgICAgRVEgLT5cbiAgICAgICAgICAgICAgICAgICAgSnVzdCB2YWx1ZVxuXG4gICAgICAgICAgICAgICAgR1QgLT5cbiAgICAgICAgICAgICAgICAgICAgZ2V0IHRhcmdldEtleSByaWdodFxuXG5cbnstfCBEZXRlcm1pbmUgaWYgYSBrZXkgaXMgaW4gYSBkaWN0aW9uYXJ5LlxuLX1cbm1lbWJlciA6IGNvbXBhcmFibGUgLT4gRGljdCBjb21wYXJhYmxlIHYgLT4gQm9vbFxubWVtYmVyIGtleSBkaWN0ID1cbiAgICBjYXNlIGdldCBrZXkgZGljdCBvZlxuICAgICAgICBKdXN0IF8gLT5cbiAgICAgICAgICAgIFRydWVcblxuICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICBGYWxzZVxuXG5cbnstfCBEZXRlcm1pbmUgdGhlIG51bWJlciBvZiBrZXktdmFsdWUgcGFpcnMgaW4gdGhlIGRpY3Rpb25hcnkuXG4tfVxuY291bnQgOiBEaWN0IGsgdiAtPiBJbnRcbmNvdW50IGRpY3QgPVxuICAgIGNvdW50SGVscCAwIGRpY3RcblxuXG5jb3VudEhlbHAgOiBJbnQgLT4gRGljdCBrIHYgLT4gSW50XG5jb3VudEhlbHAgbiBkaWN0ID1cbiAgICBjYXNlIGRpY3Qgb2ZcbiAgICAgICAgUkJFbXB0eV9ncmVuX2J1aWx0aW4gLT5cbiAgICAgICAgICAgIG5cblxuICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIF8gXyBfIGxlZnQgcmlnaHQgLT5cbiAgICAgICAgICAgIGNvdW50SGVscCAoY291bnRIZWxwIChuICsgMSkgcmlnaHQpIGxlZnRcblxuXG57LXwgUmV0cmlldmUgdGhlIGZpcnN0LCBvciBsb3dlc3QsIGtleS12YWx1ZSBwYWlyLlxuLX1cbmZpcnN0IDogRGljdCBrIHYgLT4gTWF5YmUgeyBrZXkgOiBrLCB2YWx1ZSA6IHYgfVxuZmlyc3QgZGljdCA9XG4gICAgY2FzZSBkaWN0IG9mXG4gICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluIC0+XG4gICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBfIGsgdiBSQkVtcHR5X2dyZW5fYnVpbHRpbiBfIC0+XG4gICAgICAgICAgICBKdXN0IHsga2V5ID0gaywgdmFsdWUgPSB2IH1cblxuICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIF8gXyBfIGxlZnQgXyAtPlxuICAgICAgICAgICAgZmlyc3QgbGVmdFxuXG5cbnstfCBSZXRyaWV2ZSB0aGUgbGFzdCwgb3IgaGlnaGVzdCwga2V5LXZhbHVlIHBhaXIuXG4tfVxubGFzdCA6IERpY3QgayB2IC0+IE1heWJlIHsga2V5IDogaywgdmFsdWUgOiB2IH1cbmxhc3QgZGljdCA9XG4gICAgY2FzZSBkaWN0IG9mXG4gICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluIC0+XG4gICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBfIGsgdiBfIFJCRW1wdHlfZ3Jlbl9idWlsdGluIC0+XG4gICAgICAgICAgICBKdXN0IHsga2V5ID0gaywgdmFsdWUgPSB2IH1cblxuICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIF8gXyBfIF8gcmlnaHQgLT5cbiAgICAgICAgICAgIGxhc3QgcmlnaHRcblxuXG57LXwgRmluZCB0aGUgZmlyc3Qga2V5LXZhbHVlIHBhaXIgdGhhdCBwYXNzZXMgdGhlIHRlc3QuXG4tfVxuZmluZEZpcnN0IDogKGsgLT4gdiAtPiBCb29sKSAtPiBEaWN0IGsgdiAtPiBNYXliZSB7IGtleSA6IGssIHZhbHVlIDogdiB9XG5maW5kRmlyc3QgZm4gZGljdCA9XG4gICAgY2FzZSBkaWN0IG9mXG4gICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluIC0+XG4gICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBfIGtleSB2YWx1ZSBsZWZ0IHJpZ2h0IC0+XG4gICAgICAgICAgICBjYXNlIGZpbmRGaXJzdCBmbiBsZWZ0IG9mXG4gICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICBpZiBmbiBrZXkgdmFsdWUgdGhlblxuICAgICAgICAgICAgICAgICAgICAgICAgSnVzdCB7IGtleSA9IGtleSwgdmFsdWUgPSB2YWx1ZSB9XG5cbiAgICAgICAgICAgICAgICAgICAgZWxzZVxuICAgICAgICAgICAgICAgICAgICAgICAgZmluZEZpcnN0IGZuIHJpZ2h0XG5cbiAgICAgICAgICAgICAgICBmb3VuZFZhbHVlIC0+XG4gICAgICAgICAgICAgICAgICAgIGZvdW5kVmFsdWVcblxuXG57LXwgRmluZCB0aGUgbGFzdCBrZXktdmFsdWUgcGFpciB0aGF0IHBhc3NlcyB0aGUgdGVzdC5cbi19XG5maW5kTGFzdCA6IChrIC0+IHYgLT4gQm9vbCkgLT4gRGljdCBrIHYgLT4gTWF5YmUgeyBrZXkgOiBrLCB2YWx1ZSA6IHYgfVxuZmluZExhc3QgZm4gZGljdCA9XG4gICAgY2FzZSBkaWN0IG9mXG4gICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluIC0+XG4gICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBfIGtleSB2YWx1ZSBsZWZ0IHJpZ2h0IC0+XG4gICAgICAgICAgICBjYXNlIGZpbmRMYXN0IGZuIHJpZ2h0IG9mXG4gICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICBpZiBmbiBrZXkgdmFsdWUgdGhlblxuICAgICAgICAgICAgICAgICAgICAgICAgSnVzdCB7IGtleSA9IGtleSwgdmFsdWUgPSB2YWx1ZSB9XG5cbiAgICAgICAgICAgICAgICAgICAgZWxzZVxuICAgICAgICAgICAgICAgICAgICAgICAgZmluZExhc3QgZm4gbGVmdFxuXG4gICAgICAgICAgICAgICAgZm91bmRWYWx1ZSAtPlxuICAgICAgICAgICAgICAgICAgICBmb3VuZFZhbHVlXG5cblxuey18IENoZWNrcyBpZiBhbnkga2V5LXZhbHVlIHBhaXIgaW4gdGhlIGRpY3Rpb25hcnkgcGFzc2VzIHRoZSB0ZXN0LlxuLX1cbmFueSA6IChrIC0+IHYgLT4gQm9vbCkgLT4gRGljdCBrIHYgLT4gQm9vbFxuYW55IGZuIGRpY3QgPVxuICAgIGNhc2UgZmluZEZpcnN0IGZuIGRpY3Qgb2ZcbiAgICAgICAgSnVzdCBfIC0+XG4gICAgICAgICAgICBUcnVlXG5cbiAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgRmFsc2VcblxuXG57LXwgQ2hlY2tzIGlmIGFsbCBrZXktdmFsdWUgcGFpcnMgaW4gdGhlIGRpY3Rpb25hcnkgcGFzc2VzIHRoZSB0ZXN0LlxuLX1cbmFsbCA6IChrIC0+IHYgLT4gQm9vbCkgLT4gRGljdCBrIHYgLT4gQm9vbFxuYWxsIGZuIGRpY3QgPVxuICAgIGNhc2UgZmluZEZpcnN0IChcXGtleSB2YWx1ZSAtPiBub3QgPHwgZm4ga2V5IHZhbHVlKSBkaWN0IG9mXG4gICAgICAgIEp1c3QgXyAtPlxuICAgICAgICAgICAgRmFsc2VcblxuICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICBUcnVlXG5cblxuey18IERldGVybWluZSBpZiBhIGRpY3Rpb25hcnkgaXMgZW1wdHkuXG5cbiAgICBpc0VtcHR5IGVtcHR5ID09IFRydWVcblxuLX1cbmlzRW1wdHkgOiBEaWN0IGsgdiAtPiBCb29sXG5pc0VtcHR5IGRpY3QgPVxuICAgIGNhc2UgZGljdCBvZlxuICAgICAgICBSQkVtcHR5X2dyZW5fYnVpbHRpbiAtPlxuICAgICAgICAgICAgVHJ1ZVxuXG4gICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gXyBfIF8gXyBfIC0+XG4gICAgICAgICAgICBGYWxzZVxuXG5cbnstfCBTZXRzIGEgdmFsdWUgZm9yIGEgZ2l2ZW4ga2V5LiBFeGlzdGluZyB2YWx1ZXMgd2lsbCBiZSByZXBsYWNlZC5cbklmIHRoZSBrZXkgaXNuJ3QgYWxyZWFkeSByZWdpc3RlcmVkLCB0aGUga2V5LXZhbHVlIHBhaXIgd2lsbCBiZSBpbnNlcnRlZC5cbi19XG5zZXQgOiBjb21wYXJhYmxlIC0+IHYgLT4gRGljdCBjb21wYXJhYmxlIHYgLT4gRGljdCBjb21wYXJhYmxlIHZcbnNldCBrZXkgdmFsdWUgZGljdCA9XG4gICAgLS0gUm9vdCBub2RlIGlzIGFsd2F5cyBCbGFja1xuICAgIGNhc2Ugc2V0SGVscCBrZXkgdmFsdWUgZGljdCBvZlxuICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBrIHYgbCByIC0+XG4gICAgICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIEJsYWNrIGsgdiBsIHJcblxuICAgICAgICB4IC0+XG4gICAgICAgICAgICB4XG5cblxuc2V0SGVscCA6IGNvbXBhcmFibGUgLT4gdiAtPiBEaWN0IGNvbXBhcmFibGUgdiAtPiBEaWN0IGNvbXBhcmFibGUgdlxuc2V0SGVscCBrZXkgdmFsdWUgZGljdCA9XG4gICAgY2FzZSBkaWN0IG9mXG4gICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluIC0+XG4gICAgICAgICAgICAtLSBOZXcgbm9kZXMgYXJlIGFsd2F5cyByZWQuIElmIGl0IHZpb2xhdGVzIHRoZSBydWxlcywgaXQgd2lsbCBiZSBmaXhlZFxuICAgICAgICAgICAgLS0gd2hlbiBiYWxhbmNpbmcuXG4gICAgICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBrZXkgdmFsdWUgUkJFbXB0eV9ncmVuX2J1aWx0aW4gUkJFbXB0eV9ncmVuX2J1aWx0aW5cblxuICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIG5Db2xvciBuS2V5IG5WYWx1ZSBuTGVmdCBuUmlnaHQgLT5cbiAgICAgICAgICAgIGNhc2UgY29tcGFyZSBrZXkgbktleSBvZlxuICAgICAgICAgICAgICAgIExUIC0+XG4gICAgICAgICAgICAgICAgICAgIGJhbGFuY2UgbkNvbG9yIG5LZXkgblZhbHVlIChzZXRIZWxwIGtleSB2YWx1ZSBuTGVmdCkgblJpZ2h0XG5cbiAgICAgICAgICAgICAgICBFUSAtPlxuICAgICAgICAgICAgICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIG5Db2xvciBuS2V5IHZhbHVlIG5MZWZ0IG5SaWdodFxuXG4gICAgICAgICAgICAgICAgR1QgLT5cbiAgICAgICAgICAgICAgICAgICAgYmFsYW5jZSBuQ29sb3IgbktleSBuVmFsdWUgbkxlZnQgKHNldEhlbHAga2V5IHZhbHVlIG5SaWdodClcblxuXG5iYWxhbmNlIDogTkNvbG9yIC0+IGsgLT4gdiAtPiBEaWN0IGsgdiAtPiBEaWN0IGsgdiAtPiBEaWN0IGsgdlxuYmFsYW5jZSBjb2xvciBrZXkgdmFsdWUgbGVmdCByaWdodCA9XG4gICAgY2FzZSByaWdodCBvZlxuICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBySyByViByTGVmdCByUmlnaHQgLT5cbiAgICAgICAgICAgIGNhc2UgbGVmdCBvZlxuICAgICAgICAgICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gUmVkIGxLIGxWIGxMZWZ0IGxSaWdodCAtPlxuICAgICAgICAgICAgICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluXG4gICAgICAgICAgICAgICAgICAgICAgICBSZWRcbiAgICAgICAgICAgICAgICAgICAgICAgIGtleVxuICAgICAgICAgICAgICAgICAgICAgICAgdmFsdWVcbiAgICAgICAgICAgICAgICAgICAgICAgIChSQk5vZGVfZ3Jlbl9idWlsdGluIEJsYWNrIGxLIGxWIGxMZWZ0IGxSaWdodClcbiAgICAgICAgICAgICAgICAgICAgICAgIChSQk5vZGVfZ3Jlbl9idWlsdGluIEJsYWNrIHJLIHJWIHJMZWZ0IHJSaWdodClcblxuICAgICAgICAgICAgICAgIF8gLT5cbiAgICAgICAgICAgICAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBjb2xvciBySyByViAoUkJOb2RlX2dyZW5fYnVpbHRpbiBSZWQga2V5IHZhbHVlIGxlZnQgckxlZnQpIHJSaWdodFxuXG4gICAgICAgIF8gLT5cbiAgICAgICAgICAgIGNhc2UgbGVmdCBvZlxuICAgICAgICAgICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gUmVkIGxLIGxWIChSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBsbEsgbGxWIGxsTGVmdCBsbFJpZ2h0KSBsUmlnaHQgLT5cbiAgICAgICAgICAgICAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpblxuICAgICAgICAgICAgICAgICAgICAgICAgUmVkXG4gICAgICAgICAgICAgICAgICAgICAgICBsS1xuICAgICAgICAgICAgICAgICAgICAgICAgbFZcbiAgICAgICAgICAgICAgICAgICAgICAgIChSQk5vZGVfZ3Jlbl9idWlsdGluIEJsYWNrIGxsSyBsbFYgbGxMZWZ0IGxsUmlnaHQpXG4gICAgICAgICAgICAgICAgICAgICAgICAoUkJOb2RlX2dyZW5fYnVpbHRpbiBCbGFjayBrZXkgdmFsdWUgbFJpZ2h0IHJpZ2h0KVxuXG4gICAgICAgICAgICAgICAgXyAtPlxuICAgICAgICAgICAgICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIGNvbG9yIGtleSB2YWx1ZSBsZWZ0IHJpZ2h0XG5cblxuey18IFJlbW92ZSBhIGtleS12YWx1ZSBwYWlyIGZyb20gYSBkaWN0aW9uYXJ5LiBJZiB0aGUga2V5IGlzIG5vdCBmb3VuZCxcbm5vIGNoYW5nZXMgYXJlIG1hZGUuXG4tfVxucmVtb3ZlIDogY29tcGFyYWJsZSAtPiBEaWN0IGNvbXBhcmFibGUgdiAtPiBEaWN0IGNvbXBhcmFibGUgdlxucmVtb3ZlIGtleSBkaWN0ID1cbiAgICAtLSBSb290IG5vZGUgaXMgYWx3YXlzIEJsYWNrXG4gICAgY2FzZSByZW1vdmVIZWxwIGtleSBkaWN0IG9mXG4gICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gUmVkIGsgdiBsIHIgLT5cbiAgICAgICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gQmxhY2sgayB2IGwgclxuXG4gICAgICAgIHggLT5cbiAgICAgICAgICAgIHhcblxuXG57LXwgVGhlIGVhc2llc3QgdGhpbmcgdG8gcmVtb3ZlIGZyb20gdGhlIHRyZWUsIGlzIGEgcmVkIG5vZGUuIEhvd2V2ZXIsIHdoZW4gc2VhcmNoaW5nIGZvciB0aGVcbm5vZGUgdG8gcmVtb3ZlLCB3ZSBoYXZlIG5vIHdheSBvZiBrbm93aW5nIGlmIGl0IHdpbGwgYmUgcmVkIG9yIG5vdC4gVGhpcyByZW1vdmUgaW1wbGVtZW50YXRpb25cbm1ha2VzIHN1cmUgdGhhdCB0aGUgYm90dG9tIG5vZGUgaXMgcmVkIGJ5IG1vdmluZyByZWQgY29sb3JzIGRvd24gdGhlIHRyZWUgdGhyb3VnaCByb3RhdGlvblxuYW5kIGNvbG9yIGZsaXBzLiBBbnkgdmlvbGF0aW9ucyB0aGlzIHdpbGwgY2F1c2UsIGNhbiBlYXNpbHkgYmUgZml4ZWQgYnkgYmFsYW5jaW5nIG9uIHRoZSB3YXlcbnVwIGFnYWluLlxuLX1cbnJlbW92ZUhlbHAgOiBjb21wYXJhYmxlIC0+IERpY3QgY29tcGFyYWJsZSB2IC0+IERpY3QgY29tcGFyYWJsZSB2XG5yZW1vdmVIZWxwIHRhcmdldEtleSBkaWN0ID1cbiAgICBjYXNlIGRpY3Qgb2ZcbiAgICAgICAgUkJFbXB0eV9ncmVuX2J1aWx0aW4gLT5cbiAgICAgICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluXG5cbiAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBjb2xvciBrZXkgdmFsdWUgbGVmdCByaWdodCAtPlxuICAgICAgICAgICAgaWYgdGFyZ2V0S2V5IDwga2V5IHRoZW5cbiAgICAgICAgICAgICAgICBjYXNlIGxlZnQgb2ZcbiAgICAgICAgICAgICAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBCbGFjayBfIF8gbExlZnQgXyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgY2FzZSBsTGVmdCBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gUmVkIF8gXyBfIF8gLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBjb2xvciBrZXkgdmFsdWUgKHJlbW92ZUhlbHAgdGFyZ2V0S2V5IGxlZnQpIHJpZ2h0XG5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBfIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNhc2UgbW92ZVJlZExlZnQgZGljdCBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBuQ29sb3IgbktleSBuVmFsdWUgbkxlZnQgblJpZ2h0IC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYmFsYW5jZSBuQ29sb3IgbktleSBuVmFsdWUgKHJlbW92ZUhlbHAgdGFyZ2V0S2V5IG5MZWZ0KSBuUmlnaHRcblxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgUkJFbXB0eV9ncmVuX2J1aWx0aW4gLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBSQkVtcHR5X2dyZW5fYnVpbHRpblxuXG4gICAgICAgICAgICAgICAgICAgIF8gLT5cbiAgICAgICAgICAgICAgICAgICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gY29sb3Iga2V5IHZhbHVlIChyZW1vdmVIZWxwIHRhcmdldEtleSBsZWZ0KSByaWdodFxuXG4gICAgICAgICAgICBlbHNlXG4gICAgICAgICAgICAgICAgcmVtb3ZlSGVscEVRR1QgdGFyZ2V0S2V5IChyZW1vdmVIZWxwUHJlcEVRR1QgdGFyZ2V0S2V5IGRpY3QgY29sb3Iga2V5IHZhbHVlIGxlZnQgcmlnaHQpXG5cblxucmVtb3ZlSGVscFByZXBFUUdUIDogY29tcGFyYWJsZSAtPiBEaWN0IGNvbXBhcmFibGUgdiAtPiBOQ29sb3IgLT4gY29tcGFyYWJsZSAtPiB2IC0+IERpY3QgY29tcGFyYWJsZSB2IC0+IERpY3QgY29tcGFyYWJsZSB2IC0+IERpY3QgY29tcGFyYWJsZSB2XG5yZW1vdmVIZWxwUHJlcEVRR1QgdGFyZ2V0S2V5IGRpY3QgY29sb3Iga2V5IHZhbHVlIGxlZnQgcmlnaHQgPVxuICAgIGNhc2UgbGVmdCBvZlxuICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBsSyBsViBsTGVmdCBsUmlnaHQgLT5cbiAgICAgICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW5cbiAgICAgICAgICAgICAgICBjb2xvclxuICAgICAgICAgICAgICAgIGxLXG4gICAgICAgICAgICAgICAgbFZcbiAgICAgICAgICAgICAgICBsTGVmdFxuICAgICAgICAgICAgICAgIChSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBrZXkgdmFsdWUgbFJpZ2h0IHJpZ2h0KVxuXG4gICAgICAgIF8gLT5cbiAgICAgICAgICAgIGNhc2UgcmlnaHQgb2ZcbiAgICAgICAgICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIEJsYWNrIF8gXyAoUkJOb2RlX2dyZW5fYnVpbHRpbiBCbGFjayBfIF8gXyBfKSBfIC0+XG4gICAgICAgICAgICAgICAgICAgIG1vdmVSZWRSaWdodCBkaWN0XG5cbiAgICAgICAgICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIEJsYWNrIF8gXyBSQkVtcHR5X2dyZW5fYnVpbHRpbiBfIC0+XG4gICAgICAgICAgICAgICAgICAgIG1vdmVSZWRSaWdodCBkaWN0XG5cbiAgICAgICAgICAgICAgICBfIC0+XG4gICAgICAgICAgICAgICAgICAgIGRpY3RcblxuXG57LXwgV2hlbiB3ZSBmaW5kIHRoZSBub2RlIHdlIGFyZSBsb29raW5nIGZvciwgd2UgY2FuIHJlbW92ZSBieSByZXBsYWNpbmcgdGhlIGtleS12YWx1ZVxucGFpciB3aXRoIHRoZSBrZXktdmFsdWUgcGFpciBvZiB0aGUgbGVmdC1tb3N0IG5vZGUgb24gdGhlIHJpZ2h0IHNpZGUgKHRoZSBjbG9zZXN0IHBhaXIpLlxuLX1cbnJlbW92ZUhlbHBFUUdUIDogY29tcGFyYWJsZSAtPiBEaWN0IGNvbXBhcmFibGUgdiAtPiBEaWN0IGNvbXBhcmFibGUgdlxucmVtb3ZlSGVscEVRR1QgdGFyZ2V0S2V5IGRpY3QgPVxuICAgIGNhc2UgZGljdCBvZlxuICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIGNvbG9yIGtleSB2YWx1ZSBsZWZ0IHJpZ2h0IC0+XG4gICAgICAgICAgICBpZiB0YXJnZXRLZXkgPT0ga2V5IHRoZW5cbiAgICAgICAgICAgICAgICBjYXNlIGdldE1pbiByaWdodCBvZlxuICAgICAgICAgICAgICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIF8gbWluS2V5IG1pblZhbHVlIF8gXyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgYmFsYW5jZSBjb2xvciBtaW5LZXkgbWluVmFsdWUgbGVmdCAocmVtb3ZlTWluIHJpZ2h0KVxuXG4gICAgICAgICAgICAgICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICBSQkVtcHR5X2dyZW5fYnVpbHRpblxuXG4gICAgICAgICAgICBlbHNlXG4gICAgICAgICAgICAgICAgYmFsYW5jZSBjb2xvciBrZXkgdmFsdWUgbGVmdCAocmVtb3ZlSGVscCB0YXJnZXRLZXkgcmlnaHQpXG5cbiAgICAgICAgUkJFbXB0eV9ncmVuX2J1aWx0aW4gLT5cbiAgICAgICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluXG5cblxuZ2V0TWluIDogRGljdCBrIHYgLT4gRGljdCBrIHZcbmdldE1pbiBkaWN0ID1cbiAgICBjYXNlIGRpY3Qgb2ZcbiAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBfIF8gXyAoKFJCTm9kZV9ncmVuX2J1aWx0aW4gXyBfIF8gXyBfKSBhcyBsZWZ0KSBfIC0+XG4gICAgICAgICAgICBnZXRNaW4gbGVmdFxuXG4gICAgICAgIF8gLT5cbiAgICAgICAgICAgIGRpY3RcblxuXG5yZW1vdmVNaW4gOiBEaWN0IGsgdiAtPiBEaWN0IGsgdlxucmVtb3ZlTWluIGRpY3QgPVxuICAgIGNhc2UgZGljdCBvZlxuICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIGNvbG9yIGtleSB2YWx1ZSAoKFJCTm9kZV9ncmVuX2J1aWx0aW4gbENvbG9yIF8gXyBsTGVmdCBfKSBhcyBsZWZ0KSByaWdodCAtPlxuICAgICAgICAgICAgY2FzZSBsQ29sb3Igb2ZcbiAgICAgICAgICAgICAgICBCbGFjayAtPlxuICAgICAgICAgICAgICAgICAgICBjYXNlIGxMZWZ0IG9mXG4gICAgICAgICAgICAgICAgICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBfIF8gXyBfIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBjb2xvciBrZXkgdmFsdWUgKHJlbW92ZU1pbiBsZWZ0KSByaWdodFxuXG4gICAgICAgICAgICAgICAgICAgICAgICBfIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgY2FzZSBtb3ZlUmVkTGVmdCBkaWN0IG9mXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gbkNvbG9yIG5LZXkgblZhbHVlIG5MZWZ0IG5SaWdodCAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYmFsYW5jZSBuQ29sb3IgbktleSBuVmFsdWUgKHJlbW92ZU1pbiBuTGVmdCkgblJpZ2h0XG5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgUkJFbXB0eV9ncmVuX2J1aWx0aW4gLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluXG5cbiAgICAgICAgICAgICAgICBfIC0+XG4gICAgICAgICAgICAgICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gY29sb3Iga2V5IHZhbHVlIChyZW1vdmVNaW4gbGVmdCkgcmlnaHRcblxuICAgICAgICBfIC0+XG4gICAgICAgICAgICBSQkVtcHR5X2dyZW5fYnVpbHRpblxuXG5cbm1vdmVSZWRMZWZ0IDogRGljdCBrIHYgLT4gRGljdCBrIHZcbm1vdmVSZWRMZWZ0IGRpY3QgPVxuICAgIGNhc2UgZGljdCBvZlxuICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIGNsciBrIHYgKFJCTm9kZV9ncmVuX2J1aWx0aW4gbENsciBsSyBsViBsTGVmdCBsUmlnaHQpIChSQk5vZGVfZ3Jlbl9idWlsdGluIHJDbHIgcksgclYgKChSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBybEsgcmxWIHJsTCBybFIpIGFzIHJMZWZ0KSByUmlnaHQpIC0+XG4gICAgICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluXG4gICAgICAgICAgICAgICAgUmVkXG4gICAgICAgICAgICAgICAgcmxLXG4gICAgICAgICAgICAgICAgcmxWXG4gICAgICAgICAgICAgICAgKFJCTm9kZV9ncmVuX2J1aWx0aW4gQmxhY2sgayB2IChSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBsSyBsViBsTGVmdCBsUmlnaHQpIHJsTClcbiAgICAgICAgICAgICAgICAoUkJOb2RlX2dyZW5fYnVpbHRpbiBCbGFjayBySyByViBybFIgclJpZ2h0KVxuXG4gICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gY2xyIGsgdiAoUkJOb2RlX2dyZW5fYnVpbHRpbiBsQ2xyIGxLIGxWIGxMZWZ0IGxSaWdodCkgKFJCTm9kZV9ncmVuX2J1aWx0aW4gckNsciBySyByViByTGVmdCByUmlnaHQpIC0+XG4gICAgICAgICAgICBjYXNlIGNsciBvZlxuICAgICAgICAgICAgICAgIEJsYWNrIC0+XG4gICAgICAgICAgICAgICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW5cbiAgICAgICAgICAgICAgICAgICAgICAgIEJsYWNrXG4gICAgICAgICAgICAgICAgICAgICAgICBrXG4gICAgICAgICAgICAgICAgICAgICAgICB2XG4gICAgICAgICAgICAgICAgICAgICAgICAoUkJOb2RlX2dyZW5fYnVpbHRpbiBSZWQgbEsgbFYgbExlZnQgbFJpZ2h0KVxuICAgICAgICAgICAgICAgICAgICAgICAgKFJCTm9kZV9ncmVuX2J1aWx0aW4gUmVkIHJLIHJWIHJMZWZ0IHJSaWdodClcblxuICAgICAgICAgICAgICAgIFJlZCAtPlxuICAgICAgICAgICAgICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluXG4gICAgICAgICAgICAgICAgICAgICAgICBCbGFja1xuICAgICAgICAgICAgICAgICAgICAgICAga1xuICAgICAgICAgICAgICAgICAgICAgICAgdlxuICAgICAgICAgICAgICAgICAgICAgICAgKFJCTm9kZV9ncmVuX2J1aWx0aW4gUmVkIGxLIGxWIGxMZWZ0IGxSaWdodClcbiAgICAgICAgICAgICAgICAgICAgICAgIChSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBySyByViByTGVmdCByUmlnaHQpXG5cbiAgICAgICAgXyAtPlxuICAgICAgICAgICAgZGljdFxuXG5cbm1vdmVSZWRSaWdodCA6IERpY3QgayB2IC0+IERpY3QgayB2XG5tb3ZlUmVkUmlnaHQgZGljdCA9XG4gICAgY2FzZSBkaWN0IG9mXG4gICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gY2xyIGsgdiAoUkJOb2RlX2dyZW5fYnVpbHRpbiBsQ2xyIGxLIGxWIChSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBsbEsgbGxWIGxsTGVmdCBsbFJpZ2h0KSBsUmlnaHQpIChSQk5vZGVfZ3Jlbl9idWlsdGluIHJDbHIgcksgclYgckxlZnQgclJpZ2h0KSAtPlxuICAgICAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpblxuICAgICAgICAgICAgICAgIFJlZFxuICAgICAgICAgICAgICAgIGxLXG4gICAgICAgICAgICAgICAgbFZcbiAgICAgICAgICAgICAgICAoUkJOb2RlX2dyZW5fYnVpbHRpbiBCbGFjayBsbEsgbGxWIGxsTGVmdCBsbFJpZ2h0KVxuICAgICAgICAgICAgICAgIChSQk5vZGVfZ3Jlbl9idWlsdGluIEJsYWNrIGsgdiBsUmlnaHQgKFJCTm9kZV9ncmVuX2J1aWx0aW4gUmVkIHJLIHJWIHJMZWZ0IHJSaWdodCkpXG5cbiAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBjbHIgayB2IChSQk5vZGVfZ3Jlbl9idWlsdGluIGxDbHIgbEsgbFYgbExlZnQgbFJpZ2h0KSAoUkJOb2RlX2dyZW5fYnVpbHRpbiByQ2xyIHJLIHJWIHJMZWZ0IHJSaWdodCkgLT5cbiAgICAgICAgICAgIGNhc2UgY2xyIG9mXG4gICAgICAgICAgICAgICAgQmxhY2sgLT5cbiAgICAgICAgICAgICAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpblxuICAgICAgICAgICAgICAgICAgICAgICAgQmxhY2tcbiAgICAgICAgICAgICAgICAgICAgICAgIGtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZcbiAgICAgICAgICAgICAgICAgICAgICAgIChSQk5vZGVfZ3Jlbl9idWlsdGluIFJlZCBsSyBsViBsTGVmdCBsUmlnaHQpXG4gICAgICAgICAgICAgICAgICAgICAgICAoUkJOb2RlX2dyZW5fYnVpbHRpbiBSZWQgcksgclYgckxlZnQgclJpZ2h0KVxuXG4gICAgICAgICAgICAgICAgUmVkIC0+XG4gICAgICAgICAgICAgICAgICAgIFJCTm9kZV9ncmVuX2J1aWx0aW5cbiAgICAgICAgICAgICAgICAgICAgICAgIEJsYWNrXG4gICAgICAgICAgICAgICAgICAgICAgICBrXG4gICAgICAgICAgICAgICAgICAgICAgICB2XG4gICAgICAgICAgICAgICAgICAgICAgICAoUkJOb2RlX2dyZW5fYnVpbHRpbiBSZWQgbEsgbFYgbExlZnQgbFJpZ2h0KVxuICAgICAgICAgICAgICAgICAgICAgICAgKFJCTm9kZV9ncmVuX2J1aWx0aW4gUmVkIHJLIHJWIHJMZWZ0IHJSaWdodClcblxuICAgICAgICBfIC0+XG4gICAgICAgICAgICBkaWN0XG5cblxuey18IFVwZGF0ZSB0aGUgdmFsdWUgb2YgYSBkaWN0aW9uYXJ5IGZvciBhIHNwZWNpZmljIGtleSB3aXRoIGEgZ2l2ZW4gZnVuY3Rpb24uXG4tfVxudXBkYXRlIDogY29tcGFyYWJsZSAtPiAoTWF5YmUgdiAtPiBNYXliZSB2KSAtPiBEaWN0IGNvbXBhcmFibGUgdiAtPiBEaWN0IGNvbXBhcmFibGUgdlxudXBkYXRlIHRhcmdldEtleSBhbHRlciBkaWN0aW9uYXJ5ID1cbiAgICBjYXNlIGFsdGVyIChnZXQgdGFyZ2V0S2V5IGRpY3Rpb25hcnkpIG9mXG4gICAgICAgIEp1c3QgdmFsdWUgLT5cbiAgICAgICAgICAgIHNldCB0YXJnZXRLZXkgdmFsdWUgZGljdGlvbmFyeVxuXG4gICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgIHJlbW92ZSB0YXJnZXRLZXkgZGljdGlvbmFyeVxuXG5cbnstfCBDcmVhdGUgYSBkaWN0aW9uYXJ5IHdpdGggb25lIGtleS12YWx1ZSBwYWlyLlxuLX1cbnNpbmdsZXRvbiA6IGNvbXBhcmFibGUgLT4gdiAtPiBEaWN0IGNvbXBhcmFibGUgdlxuc2luZ2xldG9uIGtleSB2YWx1ZSA9XG4gICAgLS0gUm9vdCBub2RlIGlzIGFsd2F5cyBCbGFja1xuICAgIFJCTm9kZV9ncmVuX2J1aWx0aW4gQmxhY2sga2V5IHZhbHVlIFJCRW1wdHlfZ3Jlbl9idWlsdGluIFJCRW1wdHlfZ3Jlbl9idWlsdGluXG5cblxuXG4tLSBDT01CSU5FXG5cblxuey18IENvbWJpbmUgdHdvIGRpY3Rpb25hcmllcy4gSWYgdGhlcmUgaXMgYSBjb2xsaXNpb24sIHByZWZlcmVuY2UgaXMgZ2l2ZW5cbnRvIHRoZSBmaXJzdCBkaWN0aW9uYXJ5LlxuLX1cbnVuaW9uIDogRGljdCBjb21wYXJhYmxlIHYgLT4gRGljdCBjb21wYXJhYmxlIHYgLT4gRGljdCBjb21wYXJhYmxlIHZcbnVuaW9uIHQxIHQyID1cbiAgICBmb2xkbCBzZXQgdDIgdDFcblxuXG57LXwgS2VlcCBhIGtleS12YWx1ZSBwYWlyIHdoZW4gaXRzIGtleSBhcHBlYXJzIGluIHRoZSBzZWNvbmQgZGljdGlvbmFyeS5cblByZWZlcmVuY2UgaXMgZ2l2ZW4gdG8gdmFsdWVzIGluIHRoZSBmaXJzdCBkaWN0aW9uYXJ5LlxuLX1cbmludGVyc2VjdCA6IERpY3QgY29tcGFyYWJsZSB2IC0+IERpY3QgY29tcGFyYWJsZSB2IC0+IERpY3QgY29tcGFyYWJsZSB2XG5pbnRlcnNlY3QgdDEgdDIgPVxuICAgIGZpbHRlciAoXFxrIF8gLT4gbWVtYmVyIGsgdDIpIHQxXG5cblxuey18IEtlZXAgYSBrZXktdmFsdWUgcGFpciB3aGVuIGl0cyBrZXkgZG9lcyBub3QgYXBwZWFyIGluIHRoZSBzZWNvbmQgZGljdGlvbmFyeS5cbi19XG5kaWZmIDogRGljdCBjb21wYXJhYmxlIGEgLT4gRGljdCBjb21wYXJhYmxlIGIgLT4gRGljdCBjb21wYXJhYmxlIGFcbmRpZmYgdDEgdDIgPVxuICAgIGZvbGRsIChcXGsgdiB0IC0+IHJlbW92ZSBrIHQpIHQxIHQyXG5cblxuXG4tLSBUUkFOU0ZPUk1cblxuXG57LXwgQXBwbHkgYSBmdW5jdGlvbiB0byBhbGwgdmFsdWVzIGluIGEgZGljdGlvbmFyeS5cbi19XG5tYXAgOiAoayAtPiBhIC0+IGIpIC0+IERpY3QgayBhIC0+IERpY3QgayBiXG5tYXAgZnVuYyBkaWN0ID1cbiAgICBjYXNlIGRpY3Qgb2ZcbiAgICAgICAgUkJFbXB0eV9ncmVuX2J1aWx0aW4gLT5cbiAgICAgICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluXG5cbiAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBjb2xvciBrZXkgdmFsdWUgbGVmdCByaWdodCAtPlxuICAgICAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBjb2xvciBrZXkgKGZ1bmMga2V5IHZhbHVlKSAobWFwIGZ1bmMgbGVmdCkgKG1hcCBmdW5jIHJpZ2h0KVxuXG5cbnstfCBGb2xkIG92ZXIgdGhlIGtleS12YWx1ZSBwYWlycyBpbiBhIGRpY3Rpb25hcnkgZnJvbSBsb3dlc3Qga2V5IHRvIGhpZ2hlc3Qga2V5LlxuXG4gICAgaW1wb3J0IERpY3QgZXhwb3NpbmcgKERpY3QpXG5cbiAgICBnZXRBZ2VzIDogRGljdCBTdHJpbmcgVXNlciAtPiBBcnJheSBTdHJpbmdcbiAgICBnZXRBZ2VzIHVzZXJzID1cbiAgICAgICAgRGljdC5mb2xkbCBhZGRBZ2UgW10gdXNlcnNcblxuICAgIGFkZEFnZSA6IFN0cmluZyAtPiBVc2VyIC0+IEFycmF5IFN0cmluZyAtPiBBcnJheSBTdHJpbmdcbiAgICBhZGRBZ2UgXyB1c2VyIGFnZXMgPVxuICAgICAgICB1c2VyLmFnZSA6OiBhZ2VzXG5cbiAgICAtLSBnZXRBZ2VzIHVzZXJzID09IFszMywxOSwyOF1cblxuLX1cbmZvbGRsIDogKGsgLT4gdiAtPiBiIC0+IGIpIC0+IGIgLT4gRGljdCBrIHYgLT4gYlxuZm9sZGwgZnVuYyBhY2MgZGljdCA9XG4gICAgY2FzZSBkaWN0IG9mXG4gICAgICAgIFJCRW1wdHlfZ3Jlbl9idWlsdGluIC0+XG4gICAgICAgICAgICBhY2NcblxuICAgICAgICBSQk5vZGVfZ3Jlbl9idWlsdGluIF8ga2V5IHZhbHVlIGxlZnQgcmlnaHQgLT5cbiAgICAgICAgICAgIGZvbGRsIGZ1bmMgKGZ1bmMga2V5IHZhbHVlIChmb2xkbCBmdW5jIGFjYyBsZWZ0KSkgcmlnaHRcblxuXG57LXwgRm9sZCBvdmVyIHRoZSBrZXktdmFsdWUgcGFpcnMgaW4gYSBkaWN0aW9uYXJ5IGZyb20gaGlnaGVzdCBrZXkgdG8gbG93ZXN0IGtleS5cblxuICAgIGltcG9ydCBEaWN0IGV4cG9zaW5nIChEaWN0KVxuXG4gICAgZ2V0QWdlcyA6IERpY3QgU3RyaW5nIFVzZXIgLT4gQXJyYXkgU3RyaW5nXG4gICAgZ2V0QWdlcyB1c2VycyA9XG4gICAgICAgIERpY3QuZm9sZHIgYWRkQWdlIFtdIHVzZXJzXG5cbiAgICBhZGRBZ2UgOiBTdHJpbmcgLT4gVXNlciAtPiBBcnJheSBTdHJpbmcgLT4gQXJyYXkgU3RyaW5nXG4gICAgYWRkQWdlIF8gdXNlciBhZ2VzID1cbiAgICAgICAgdXNlci5hZ2UgOjogYWdlc1xuXG4gICAgLS0gZ2V0QWdlcyB1c2VycyA9PSBbMjgsMTksMzNdXG5cbi19XG5mb2xkciA6IChrIC0+IHYgLT4gYiAtPiBiKSAtPiBiIC0+IERpY3QgayB2IC0+IGJcbmZvbGRyIGZ1bmMgYWNjIHQgPVxuICAgIGNhc2UgdCBvZlxuICAgICAgICBSQkVtcHR5X2dyZW5fYnVpbHRpbiAtPlxuICAgICAgICAgICAgYWNjXG5cbiAgICAgICAgUkJOb2RlX2dyZW5fYnVpbHRpbiBfIGtleSB2YWx1ZSBsZWZ0IHJpZ2h0IC0+XG4gICAgICAgICAgICBmb2xkciBmdW5jIChmdW5jIGtleSB2YWx1ZSAoZm9sZHIgZnVuYyBhY2MgcmlnaHQpKSBsZWZ0XG5cblxuey18IEtlZXAgb25seSB0aGUga2V5LXZhbHVlIHBhaXJzIHRoYXQgcGFzcyB0aGUgZ2l2ZW4gdGVzdC5cbi19XG5maWx0ZXIgOiAoY29tcGFyYWJsZSAtPiB2IC0+IEJvb2wpIC0+IERpY3QgY29tcGFyYWJsZSB2IC0+IERpY3QgY29tcGFyYWJsZSB2XG5maWx0ZXIgaXNHb29kIGRpY3QgPVxuICAgIGZvbGRsXG4gICAgICAgIChcXGsgdiBkIC0+XG4gICAgICAgICAgICBpZiBpc0dvb2QgayB2IHRoZW5cbiAgICAgICAgICAgICAgICBzZXQgayB2IGRcblxuICAgICAgICAgICAgZWxzZVxuICAgICAgICAgICAgICAgIGRcbiAgICAgICAgKVxuICAgICAgICBlbXB0eVxuICAgICAgICBkaWN0XG5cblxuey18IEZpbHRlciBvdXQgdW53YW50ZWQgcmVzdWx0cyBvZiBhIG1hcCBvcGVyYXRpb24uXG4tfVxuZmlsdGVyTWFwIDogKGNvbXBhcmFibGUgLT4gdiAtPiBNYXliZSB4KSAtPiBEaWN0IGNvbXBhcmFibGUgdiAtPiBEaWN0IGNvbXBhcmFibGUgeFxuZmlsdGVyTWFwIHRvTWF5YmUgZGljdCA9XG4gICAgZm9sZGxcbiAgICAgICAgKFxcayB2IGQgLT5cbiAgICAgICAgICAgIGNhc2UgdG9NYXliZSBrIHYgb2ZcbiAgICAgICAgICAgICAgICBKdXN0IG5ld1ZhbHVlIC0+XG4gICAgICAgICAgICAgICAgICAgIHNldCBrIG5ld1ZhbHVlIGRcblxuICAgICAgICAgICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgICAgICAgICAgZFxuICAgICAgICApXG4gICAgICAgIGVtcHR5XG4gICAgICAgIGRpY3RcblxuXG57LXwgUGFydGl0aW9uIGEgZGljdGlvbmFyeSBhY2NvcmRpbmcgdG8gc29tZSB0ZXN0LiBUaGUgZmlyc3QgZGljdGlvbmFyeVxuY29udGFpbnMgYWxsIGtleS12YWx1ZSBwYWlycyB3aGljaCBwYXNzZWQgdGhlIHRlc3QsIGFuZCB0aGUgc2Vjb25kIGNvbnRhaW5zXG50aGUgcGFpcnMgdGhhdCBkaWQgbm90LlxuLX1cbnBhcnRpdGlvbiA6IChjb21wYXJhYmxlIC0+IHYgLT4gQm9vbCkgLT4gRGljdCBjb21wYXJhYmxlIHYgLT4geyB0cnVlcyA6IERpY3QgY29tcGFyYWJsZSB2LCBmYWxzZXMgOiBEaWN0IGNvbXBhcmFibGUgdiB9XG5wYXJ0aXRpb24gaXNHb29kIGRpY3QgPVxuICAgIGxldFxuICAgICAgICBhZGQga2V5IHZhbHVlIHsgdHJ1ZXMsIGZhbHNlcyB9ID1cbiAgICAgICAgICAgIGlmIGlzR29vZCBrZXkgdmFsdWUgdGhlblxuICAgICAgICAgICAgICAgIHsgdHJ1ZXMgPSBzZXQga2V5IHZhbHVlIHRydWVzXG4gICAgICAgICAgICAgICAgLCBmYWxzZXMgPSBmYWxzZXNcbiAgICAgICAgICAgICAgICB9XG5cbiAgICAgICAgICAgIGVsc2VcbiAgICAgICAgICAgICAgICB7IHRydWVzID0gdHJ1ZXNcbiAgICAgICAgICAgICAgICAsIGZhbHNlcyA9IHNldCBrZXkgdmFsdWUgZmFsc2VzXG4gICAgICAgICAgICAgICAgfVxuICAgIGluXG4gICAgZm9sZGwgYWRkIHsgdHJ1ZXMgPSBlbXB0eSwgZmFsc2VzID0gZW1wdHkgfSBkaWN0XG5cblxuXG4tLSBMSVNUU1xuXG5cbnstfCBHZXQgYWxsIG9mIHRoZSBrZXlzIGluIGEgZGljdGlvbmFyeSwgc29ydGVkIGZyb20gbG93ZXN0IHRvIGhpZ2hlc3QuXG5cbiAgICBrZXlzIChEaWN0LmVtcHR5IHw+IERpY3Quc2V0IDAgXCJBbGljZVwiIHw+IERpY3Quc2V0IDEgXCJCb2JcIikgPT0gWyAwLCAxIF1cblxuLX1cbmtleXMgOiBEaWN0IGsgdiAtPiBBcnJheSBrXG5rZXlzIGRpY3QgPVxuICAgIGZvbGRsIChcXGtleSB2YWx1ZSBrZXlBcnJheSAtPiBBcnJheS5wdXNoTGFzdCBrZXkga2V5QXJyYXkpIFtdIGRpY3RcblxuXG57LXwgR2V0IGFsbCBvZiB0aGUgdmFsdWVzIGluIGEgZGljdGlvbmFyeSwgaW4gdGhlIG9yZGVyIG9mIHRoZWlyIGtleXMuXG5cbiAgICB2YWx1ZXMgKERpY3QuZW1wdHkgfD4gRGljdC5zZXQgMCBcIkFsaWNlXCIgfD4gRGljdC5zZXQgMSBcIkJvYlwiKSA9PSBbIFwiQWxpY2VcIiwgXCJCb2JcIiBdXG5cbi19XG52YWx1ZXMgOiBEaWN0IGsgdiAtPiBBcnJheSB2XG52YWx1ZXMgZGljdCA9XG4gICAgZm9sZGwgKFxca2V5IHZhbHVlIHZhbHVlQXJyYXkgLT4gQXJyYXkucHVzaExhc3QgdmFsdWUgdmFsdWVBcnJheSkgW10gZGljdFxuXG5cbnstfCBUaGUgbW9zdCBnZW5lcmFsIHdheSBvZiBjb21iaW5pbmcgdHdvIGRpY3Rpb25hcmllcy4gWW91IHByb3ZpZGUgdGhyZWVcbmFjY3VtdWxhdG9ycyBmb3Igd2hlbiBhIGdpdmVuIGtleSBhcHBlYXJzOlxuXG4xLiAgT25seSBpbiB0aGUgbGVmdCBkaWN0aW9uYXJ5LlxuMi4gIEluIGJvdGggZGljdGlvbmFyaWVzLlxuMy4gIE9ubHkgaW4gdGhlIHJpZ2h0IGRpY3Rpb25hcnkuXG4gICAgWW91IHRoZW4gdHJhdmVyc2UgYWxsIHRoZSBrZXlzIGZyb20gbG93ZXN0IHRvIGhpZ2hlc3QsIGJ1aWxkaW5nIHVwIHdoYXRldmVyXG4gICAgeW91IHdhbnQuXG5cbi19XG5tZXJnZSA6XG4gICAgKGNvbXBhcmFibGUgLT4gYSAtPiByZXN1bHQgLT4gcmVzdWx0KVxuICAgIC0+IChjb21wYXJhYmxlIC0+IGEgLT4gYiAtPiByZXN1bHQgLT4gcmVzdWx0KVxuICAgIC0+IChjb21wYXJhYmxlIC0+IGIgLT4gcmVzdWx0IC0+IHJlc3VsdClcbiAgICAtPiBEaWN0IGNvbXBhcmFibGUgYVxuICAgIC0+IERpY3QgY29tcGFyYWJsZSBiXG4gICAgLT4gcmVzdWx0XG4gICAgLT4gcmVzdWx0XG5tZXJnZSBsZWZ0U3RlcCBib3RoU3RlcCByaWdodFN0ZXAgbGVmdERpY3QgcmlnaHREaWN0IGluaXRpYWxSZXN1bHQgPVxuICAgIGxldFxuICAgICAgICBzdGVwU3RhdGUgcktleSByVmFsdWUgeyBsaXN0LCByZXN1bHQgfSA9XG4gICAgICAgICAgICBjYXNlIEFycmF5LnBvcEZpcnN0IGxpc3Qgb2ZcbiAgICAgICAgICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICAgICAgICAgIHsgbGlzdCA9IGxpc3RcbiAgICAgICAgICAgICAgICAgICAgLCByZXN1bHQgPSByaWdodFN0ZXAgcktleSByVmFsdWUgcmVzdWx0XG4gICAgICAgICAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgICAgIEp1c3QgeyBmaXJzdCA9IHsga2V5ID0gbEtleSwgdmFsdWUgPSBsVmFsdWUgfSwgcmVzdCB9IC0+XG4gICAgICAgICAgICAgICAgICAgIGlmIGxLZXkgPCByS2V5IHRoZW5cbiAgICAgICAgICAgICAgICAgICAgICAgIHN0ZXBTdGF0ZSByS2V5IHJWYWx1ZSBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB7IGxpc3QgPSByZXN0XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLCByZXN1bHQgPSBsZWZ0U3RlcCBsS2V5IGxWYWx1ZSByZXN1bHRcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG5cbiAgICAgICAgICAgICAgICAgICAgZWxzZSBpZiBsS2V5ID4gcktleSB0aGVuXG4gICAgICAgICAgICAgICAgICAgICAgICB7IGxpc3QgPSBsaXN0XG4gICAgICAgICAgICAgICAgICAgICAgICAsIHJlc3VsdCA9IHJpZ2h0U3RlcCByS2V5IHJWYWx1ZSByZXN1bHRcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgICAgICAgICBlbHNlXG4gICAgICAgICAgICAgICAgICAgICAgICB7IGxpc3QgPSByZXN0XG4gICAgICAgICAgICAgICAgICAgICAgICAsIHJlc3VsdCA9IGJvdGhTdGVwIGxLZXkgbFZhbHVlIHJWYWx1ZSByZXN1bHRcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cblxuICAgICAgICB7IGxpc3QgPSBsZWZ0b3ZlcnMsIHJlc3VsdCA9IGludGVybWVkaWF0ZVJlc3VsdCB9ID1cbiAgICAgICAgICAgIGZvbGRsIHN0ZXBTdGF0ZSB7IGxpc3QgPSBmb2xkbCAoXFxrZXkgdmFsdWUgYXJyYXkgLT4gQXJyYXkucHVzaExhc3QgeyBrZXkgPSBrZXksIHZhbHVlID0gdmFsdWUgfSBhcnJheSkgW10gbGVmdERpY3QsIHJlc3VsdCA9IGluaXRpYWxSZXN1bHQgfSByaWdodERpY3RcbiAgICBpblxuICAgIEFycmF5LmZvbGRsIChcXHsga2V5LCB2YWx1ZSB9IHJlc3VsdCAtPiBsZWZ0U3RlcCBrZXkgdmFsdWUgcmVzdWx0KSBpbnRlcm1lZGlhdGVSZXN1bHQgbGVmdG92ZXJzXG4iLAogICAgICAgICJtb2R1bGUgQXJyYXkgZXhwb3NpbmdcbiAgICAoIEFycmF5XG4gICAgLCBlbXB0eSwgc2luZ2xldG9uLCBpbml0aWFsaXplLCByZXBlYXQsIHJhbmdlXG4gICAgLCBtYXAsIGluZGV4ZWRNYXAsIGZvbGRsLCBmb2xkciwgaW5kZXhlZEZvbGRsLCBpbmRleGVkRm9sZHIsIGZpbHRlciwgZmlsdGVyTWFwLCByZXZlcnNlXG4gICAgLCBpc0VtcHR5LCBsZW5ndGgsIGdldCwgZmluZEZpcnN0LCBmaW5kTGFzdCwgbWVtYmVyLCBhbnksIGFsbCwgbWluaW11bSwgbWF4aW11bVxuICAgICwgc2V0LCB1cGRhdGUsIHB1c2hGaXJzdCwgcHVzaExhc3RcbiAgICAsIHByZXBlbmQsIGFwcGVuZCwgZmxhdHRlbiwgZmxhdE1hcCwgaW50ZXJzcGVyc2UsIG1hcDIsIG1hcDNcbiAgICAsIGZpcnN0LCBsYXN0LCBzbGljZSwgZHJvcEZpcnN0LCBkcm9wTGFzdCwgdGFrZUZpcnN0LCB0YWtlTGFzdCwgcG9wRmlyc3QsIHBvcExhc3QsIHBhcnRpdGlvblxuICAgICwgc29ydCwgc29ydEJ5LCBzb3J0V2l0aFxuICAgIClcblxuey18IFlvdSBjYW4gY3JlYXRlIGFuIGBBcnJheWAgdXNpbmcgdGhlIGBbMSwgMiwgM11gIHN5bnRheC4gVGhpcyBtb2R1bGUgaGFzIGEgYnVuY2ggb2ZcbmZ1bmN0aW9ucyB0byBoZWxwIHlvdSB3b3JrIHdpdGggdGhlbS5cblxuQGRvY3MgQXJyYXlcblxuXG5AZG9jcyBlbXB0eSwgc2luZ2xldG9uLCBpbml0aWFsaXplLCByZXBlYXQsIHJhbmdlXG5cblxuIyMgVHJhbnNmb3JtXG5cbkBkb2NzIG1hcCwgaW5kZXhlZE1hcCwgZm9sZGwsIGZvbGRyLCBpbmRleGVkRm9sZGwsIGluZGV4ZWRGb2xkciwgZmlsdGVyLCBmaWx0ZXJNYXAsIHJldmVyc2VcblxuXG4jIyBRdWVyeVxuXG5AZG9jcyBpc0VtcHR5LCBsZW5ndGgsIGdldCwgZmluZEZpcnN0LCBmaW5kTGFzdCwgbWVtYmVyLCBhbnksIGFsbCwgbWluaW11bSwgbWF4aW11bVxuXG5cbiMjIE1vZGlmeVxuXG5AZG9jcyBzZXQsIHVwZGF0ZSwgcHVzaEZpcnN0LCBwdXNoTGFzdFxuXG5cbiMjIENvbWJpbmVcblxuQGRvY3MgcHJlcGVuZCwgYXBwZW5kLCBmbGF0dGVuLCBmbGF0TWFwLCBpbnRlcnNwZXJzZSwgbWFwMiwgbWFwM1xuXG5cbiMjIERlY29uc3RydWN0XG5cbkBkb2NzIHNsaWNlLCBkcm9wRmlyc3QsIGRyb3BMYXN0LCB0YWtlRmlyc3QsIHRha2VMYXN0LCBwb3BGaXJzdCwgcG9wTGFzdCwgZmlyc3QsIGxhc3QsIHBhcnRpdGlvblxuXG5cbiMjIFNvcnRcblxuQGRvY3Mgc29ydCwgc29ydEJ5LCBzb3J0V2l0aFxuXG5cbi19XG5cbmltcG9ydCBCYXNpY3MgZXhwb3NpbmcgKC4uKVxuaW1wb3J0IE1heWJlIGV4cG9zaW5nIChNYXliZSguLikpXG5pbXBvcnQgR3Jlbi5LZXJuZWwuQXJyYXlcblxuXG57LXwgQW4gQXJyYXkgaXMgYW4gb3JkZXJlZCBjb2xsZWN0aW9uIG9mIGVsZW1lbnRzLlxuLX1cbnR5cGUgQXJyYXkgYVxuICAgID0gQXJyYXkgYVxuXG5cbi0tIENSRUFURVxuXG5cbnstfCBBbiBlbXB0eSBhcnJheS5cbi19XG5lbXB0eSA6IEFycmF5IGFcbmVtcHR5ID1cbiAgICBbXVxuXG5cbnstfCBDcmVhdGUgYW4gYXJyYXkgY29udGFpbmluZyBhIHNpbmdsZSB2YWx1ZS5cbi19XG5zaW5nbGV0b24gOiBhIC0+IEFycmF5IGFcbnNpbmdsZXRvbiBhID1cbiAgICBbIGEgXVxuXG5cbnstfCBDcmVhdGUgYW4gYXJyYXkgb2YgYG5gIGVsZW1lbnRzLCBjb250YWluaW5nIHRoZSBlbGVtZW50c1xucmVzdWx0aW5nIGZyb20gY2FsbGluZyBgZm5gIHdpdGggYG9mZnNldCArIGluZGV4YC5cblxuICAgIGluaXRpYWxpemUgMyA1IGlkZW50aXR5ID09IFsgNSwgNiwgNyBdXG5cbkluIHRoZSBhYm92ZSBleGFtcGxlLCB3ZSBjcmVhdGUgYW4gYXJyYXkgY29udGFpbmluZyAzIGludGVnZXJzXG5zdGFydGluZyBhdCA1LlxuLX1cbmluaXRpYWxpemUgOiBJbnQgLT4gSW50IC0+IChJbnQgLT4gYSkgLT4gQXJyYXkgYVxuaW5pdGlhbGl6ZSA9XG4gICAgR3Jlbi5LZXJuZWwuQXJyYXkuaW5pdGlhbGl6ZVxuXG5cbnstfCBDcmVhdGUgYW4gYXJyYXkgd2l0aCBgbmAgY29waWVzIG9mIGEgdmFsdWU6XG5cbiAgICByZXBlYXQgNSAzID09IFsgMywgMywgMywgMywgMyBdXG5cbi19XG5yZXBlYXQgOiBJbnQgLT4gYSAtPiBBcnJheSBhXG5yZXBlYXQgbiB2YWwgPVxuICAgIGluaXRpYWxpemUgbiAwIChcXF8gLT4gdmFsKVxuXG5cbnstfCBDcmVhdGUgYSBsaXN0IG9mIG51bWJlcnMsIGV2ZXJ5IGVsZW1lbnQgaW5jcmVhc2luZyBieSBvbmUuIFlvdSBnaXZlIHRoZSBsb3dlc3QgYW5kIGhpZ2hlc3QgbnVtYmVyIHRoYXQgc2hvdWxkIGJlIGluIHRoZSBsaXN0LlxuXG4gICAgcmFuZ2UgMyA2ID09IFszLCA0LCA1LCA2XVxuICAgIHJhbmdlIDMgMyA9PSBbM11cbiAgICByYW5nZSA2IDMgPT0gW11cblxuLX1cbnJhbmdlIDogSW50IC0+IEludCAtPiBBcnJheSBJbnRcbnJhbmdlIGZyb20gdG8gPVxuICAgIGlmIGZyb20gPiB0byB0aGVuXG4gICAgICAgIFtdXG5cbiAgICBlbHNlIGlmIGZyb20gPT0gdG8gdGhlblxuICAgICAgICBbZnJvbV1cblxuICAgIGVsc2UgXG4gICAgICAgIGluaXRpYWxpemUgKHRvIC0gZnJvbSArIDEpIGZyb20gaWRlbnRpdHlcblxuXG4tLSBUUkFOU0ZPUk1cblxuXG57LXwgQXBwbHkgYSBmdW5jdGlvbiBvbiBldmVyeSBlbGVtZW50IGluIGFuIGFycmF5LlxuXG4gICAgbWFwIG5lZ2F0ZSBbIDEsIDQsIDkgXSA9PSBbIC0xLCAtNCwgLTkgXVxuXG5TbyBgbWFwIGZ1bmMgWyBhLCBiLCBjIF1gIGlzIHRoZSBzYW1lIGFzIGBbIGZ1bmMgYSwgZnVuYyBiLCBmdW5jIGMgXWBcblxuLX1cbm1hcCA6IChhIC0+IGIpIC0+IEFycmF5IGEgLT4gQXJyYXkgYlxubWFwID1cbiAgICBHcmVuLktlcm5lbC5BcnJheS5tYXBcblxuXG57LXwgU2FtZSBhcyBgbWFwYCBidXQgdGhlIGZ1bmN0aW9uIGlzIGFsc28gYXBwbGllZCB0byB0aGUgaW5kZXggb2YgZWFjaCBlbGVtZW50LlxuXG4gICAgaW5kZXhlZE1hcCAoXFxpZHggdmFsIC0+IFtpZHgsIHZhbF0pIFsgMywgMywgMyBdID09IFsgWyAwLCAzIF0sIFsgMSwgMyBdLCBbIDIsIDMgXSBdXG5cbi19XG5pbmRleGVkTWFwIDogKEludCAtPiBhIC0+IGIpIC0+IEFycmF5IGEgLT4gQXJyYXkgYlxuaW5kZXhlZE1hcCA9XG4gICAgR3Jlbi5LZXJuZWwuQXJyYXkuaW5kZXhlZE1hcFxuXG5cbnstfCBSZWR1Y2UgdGhlIGFycmF5IGZyb20gdGhlIGxlZnQuXG5cbiAgICBmb2xkbCAoKykgMCBbIDEsIDIsIDMgXSA9PSA2XG5cblNvIGBmb2xkbCBzdGVwIHN0YXRlIFsgMSwgMiwgMyBdYCBpcyBsaWtlIHNheWluZzpcblxuICAgIHN0YXRlXG4gICAgICAgIHw+IHN0ZXAgMVxuICAgICAgICB8PiBzdGVwIDJcbiAgICAgICAgfD4gc3RlcCAzXG4tfVxuZm9sZGwgOiAoYSAtPiBiIC0+IGIpIC0+IGIgLT4gQXJyYXkgYSAtPiBiXG5mb2xkbCA9XG4gICAgR3Jlbi5LZXJuZWwuQXJyYXkuZm9sZGxcblxuXG57LXwgUmVkdWNlIHRoZSBhcnJheSBmcm9tIHRoZSByaWdodC4gU2FtZSBhcyBgZm9sZGxgIGJ1dFxudGhlIGV4ZWN1dGlvbiBvcmRlciBpcyByZXZlcnNlZC5cbi19XG5mb2xkciA6IChhIC0+IGIgLT4gYikgLT4gYiAtPiBBcnJheSBhIC0+IGJcbmZvbGRyID1cbiAgICBHcmVuLktlcm5lbC5BcnJheS5mb2xkclxuXG5cbnstfCBSZWR1Y2UgdGhlIGFycmF5IGZyb20gdGhlIGxlZnQuIFRoZSByZWR1Y2luZyBmdW5jdGlvbiBpcyBcbnBhc3NlZCB0aGUgaW5kZXggb2YgdGhlIGN1cnJlbnQgdmFsdWUuXG5cbiAgICBpbmRleGVkRm9sZGwgKFxcaWR4IHZhbCBzdW0gLT4gaWR4ICsgdmFsICsgc3VtKSAwIFsgMSwgMiwgMyBdID09IDlcblxuLX1cbmluZGV4ZWRGb2xkbCA6IChJbnQgLT4gYSAtPiBiIC0+IGIpIC0+IGIgLT4gQXJyYXkgYSAtPiBiXG5pbmRleGVkRm9sZGwgPVxuICAgIEdyZW4uS2VybmVsLkFycmF5LmluZGV4ZWRGb2xkbFxuXG5cbnstfCBSZWR1Y2UgdGhlIGFycmF5IGZyb20gdGhlIHJpZ2h0LiBUaGUgcmVkdWNpbmcgZnVuY3Rpb25cbmlzIHBhc3NlZCB0aGUgaW5kZXggb2YgdGhlIGN1cnJlbnQgdmFsdWUuIFNhbWUgYXMgYGluZGV4ZWRGb2xkbGBcbmJ1dCB0aGUgZXhlY3V0aW9uIG9yZGVyIGlzIHJldmVyc2VkLlxuLX1cbmluZGV4ZWRGb2xkciA6IChJbnQgLT4gYSAtPiBiIC0+IGIpIC0+IGIgLT4gQXJyYXkgYSAtPiBiXG5pbmRleGVkRm9sZHIgPVxuICAgIEdyZW4uS2VybmVsLkFycmF5LmluZGV4ZWRGb2xkclxuXG5cbnstfCBLZWVwIHZhbHVlcyB0aGF0IHBhc3MgdGhlIHRlc3QuXG5cbiAgICBmaWx0ZXIgKFxcbiAtPiBuIDwgMykgWyAxLCAyLCAzLCA0IF0gPT0gWyAxLCAyIF1cblxuLX1cbmZpbHRlciA6IChhIC0+IEJvb2wpIC0+IEFycmF5IGEgLT4gQXJyYXkgYVxuZmlsdGVyID1cbiAgICBHcmVuLktlcm5lbC5BcnJheS5maWx0ZXJcblxuXG57LXwgRmlsdGVyIG91dCB1bndhbnRlZCByZXN1bHRzIG9mIGEgbWFwIG9wZXJhdGlvbi5cblxuICAgIGZpbHRlck1hcCBTdHJpbmcudG9JbnQgWyBcIjNcIiwgXCJub3QgYSBudW1iZXJcIiwgXCItNVwiIF0gPT0gWyAzLCAtNSBdXG4gICAgZmlsdGVyTWFwIGlkZW50aXR5IFsgSnVzdCAxLCBOb3RoaW5nIF0gPT0gWyAxIF1cblxuLX1cbmZpbHRlck1hcCA6IChhIC0+IE1heWJlIGIpIC0+IEFycmF5IGEgLT4gQXJyYXkgYlxuZmlsdGVyTWFwIG1hcHBlciBhcnJheSA9XG4gICAgZmxhdE1hcFxuICAgICAgICAoXFx2IC0+XG4gICAgICAgICAgICBjYXNlIG1hcHBlciB2IG9mXG4gICAgICAgICAgICAgICAgSnVzdCBuZXdWYWx1ZSAtPlxuICAgICAgICAgICAgICAgICAgICBbIG5ld1ZhbHVlIF1cblxuICAgICAgICAgICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgICAgICAgICAgW11cbiAgICAgICAgKVxuICAgICAgICBhcnJheVxuXG5cbnstfCBSZXZlcnNlIGFuIGFycmF5LlxuXG4gICAgcmV2ZXJzZSBbIDEsIDIsIDMgXSA9PSBbIDMsIDIsIDEgXVxuXG4tfVxucmV2ZXJzZSA6IEFycmF5IGEgLT4gQXJyYXkgYVxucmV2ZXJzZSA9XG4gICAgR3Jlbi5LZXJuZWwuQXJyYXkucmV2ZXJzZVxuXG5cbi0tIFFVRVJZXG5cblxuey18IENoZWNrIGlmIGFuIGFycmF5IGlzIGVtcHR5LlxuXG4gICAgaXNFbXB0eSBbXSA9PSBUcnVlXG4gICAgaXNFbXB0eSBbIDEsIDIsIDMgXSA9PSBGYWxzZVxuXG4tfVxuaXNFbXB0eSA6IEFycmF5IGEgLT4gQm9vbFxuaXNFbXB0eSBhcnJheSA9XG4gICAgbGVuZ3RoIGFycmF5ID09IDBcblxuXG57LXwgUmV0dXJuIHRoZSBsZW5ndGggb2YgYW4gYXJyYXkuXG5cbiAgICBsZW5ndGggWyAxLCAyLCAzIF0gPT0gM1xuXG4tfVxubGVuZ3RoIDogQXJyYXkgYSAtPiBJbnRcbmxlbmd0aCA9XG4gICAgR3Jlbi5LZXJuZWwuQXJyYXkubGVuZ3RoXG5cblxuey18IFJldHJpZXZlIHRoZSBlbGVtZW50IGF0IGEgZ2l2ZW4gaW5kZXgsIG9yIGBOb3RoaW5nYCBpZiB0aGUgaW5kZXggaXMgb3V0IG9mIGJvdW5kcy5cbkEgbmVnYXRpdmUgaW5kZXggbG9va3MgdXAgYW4gZWxlbWVudCBpbiByZXZlcnNlIGZyb20gdGhlIGVuZCBvZiB0aGUgYXJyYXkuXG5cbiAgICBnZXQgMSBbIDEsIDIsIDMgXSA9PSBKdXN0IDJcbiAgICBnZXQgMTAgWyAxLCAyLCAzIF0gPT0gTm90aGluZ1xuICAgIGdldCAtMSBbIDEsIDIsIDMgXSA9PSBKdXN0IDNcblxuLX1cbmdldCA6IEludCAtPiBBcnJheSBhIC0+IE1heWJlIGFcbmdldCA9XG4gICAgR3Jlbi5LZXJuZWwuQXJyYXkuZ2V0XG5cblxuey18IEZpbmQgdGhlIGZpcnN0IHZhbHVlIHRoYXQgcGFzc2VzIHRoZSB0ZXN0LlxuXG4gICAgZmluZCAoXFxuIC0+IG4gPiAwKSBbIC0xLCAwLCAxLCAyIF0gPT0gSnVzdCAxXG5cbi19XG5maW5kRmlyc3QgOiAoYSAtPiBCb29sKSAtPiBBcnJheSBhIC0+IE1heWJlIGFcbmZpbmRGaXJzdCA9XG4gICAgR3Jlbi5LZXJuZWwuQXJyYXkuZmluZEZpcnN0XG5cblxuey18IEZpbmQgdGhlIGxhc3QgdmFsdWUgdGhhdCBwYXNzZXMgdGhlIHRlc3QuXG5cbiAgICBmaW5kIChcXG4gLT4gbiA+IDApIFsgLTEsIDAsIDEsIDIgXSA9PSBKdXN0IDJcblxuLX1cbmZpbmRMYXN0IDogKGEgLT4gQm9vbCkgLT4gQXJyYXkgYSAtPiBNYXliZSBhXG5maW5kTGFzdCA9XG4gICAgR3Jlbi5LZXJuZWwuQXJyYXkuZmluZExhc3RcblxuXG57LXwgRmlndXJlIG91dCB3aGV0aGVyIGFuIGFycmF5IGNvbnRhaW5zIGEgdmFsdWUuXG5cbiAgICBtZW1iZXIgOSBbMSwyLDMsNF0gPT0gRmFsc2VcbiAgICBtZW1iZXIgNCBbMSwyLDMsNF0gPT0gVHJ1ZVxuXG4tfVxubWVtYmVyIDogYSAtPiBBcnJheSBhIC0+IEJvb2xcbm1lbWJlciB2YWx1ZSBhcnJheSA9XG4gICAgY2FzZSBmaW5kRmlyc3QgKFxcdiAtPiB2ID09IHZhbHVlKSBhcnJheSBvZlxuICAgICAgICBKdXN0IF8gLT5cbiAgICAgICAgICAgIFRydWVcblxuICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICBGYWxzZVxuXG5cbnstfCBEZXRlcm1pbmUgaWYgYW55IGVsZW1lbnRzIHBhc3MgdGhlIHRlc3QuXG5cbiAgICBhbnkgaXNFdmVuIFsyLDNdID09IFRydWVcbiAgICBhbnkgaXNFdmVuIFsxLDNdID09IEZhbHNlXG4gICAgYW55IGlzRXZlbiBbXSA9PSBGYWxzZVxuXG4tfVxuYW55IDogKGEgLT4gQm9vbCkgLT4gQXJyYXkgYSAtPiBCb29sXG5hbnkgZm4gYXJyYXkgPVxuICAgIGNhc2UgZmluZEZpcnN0IGZuIGFycmF5IG9mXG4gICAgICAgIEp1c3QgXyAtPlxuICAgICAgICAgICAgVHJ1ZVxuXG4gICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgIEZhbHNlXG5cblxuey18IERldGVybWluZSBpZiBhbGwgZWxlbWVudHMgcGFzcyB0aGUgdGVzdC5cblxuICAgIGFsbCBpc0V2ZW4gWzIsNF0gPT0gVHJ1ZVxuICAgIGFsbCBpc0V2ZW4gWzIsM10gPT0gRmFsc2VcbiAgICBhbGwgaXNFdmVuIFtdID09IFRydWVcblxuLX1cbmFsbCA6IChhIC0+IEJvb2wpIC0+IEFycmF5IGEgLT4gQm9vbFxuYWxsIGZuIGFycmF5ID1cbiAgICBjYXNlIGZpbmRGaXJzdCAobm90IDw8IGZuKSBhcnJheSBvZlxuICAgICAgICBKdXN0IF8gLT5cbiAgICAgICAgICAgIEZhbHNlXG5cbiAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgVHJ1ZVxuXG5cbnstfCBGaW5kIHRoZSBtaW5pbXVtIGVsZW1lbnQgaW4gYSBub24tZW1wdHkgbGlzdC5cblxuICAgIG1pbmltdW0gWzMsMiwxXSA9PSBKdXN0IDFcbiAgICBtaW5pbXVtIFtdICAgICAgPT0gTm90aGluZ1xuXG4tfVxubWluaW11bSA6IEFycmF5IGNvbXBhcmFibGUgLT4gTWF5YmUgY29tcGFyYWJsZVxubWluaW11bSBhcnJheSA9XG4gICAgY2FzZSBmaXJzdCBhcnJheSBvZlxuICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgSnVzdCB2YWwgLT5cbiAgICAgICAgICAgIEp1c3QgPHxcbiAgICAgICAgICAgICAgICBmb2xkbFxuICAgICAgICAgICAgICAgICAgICAoXFxjdXJyZW50IGxvd2VzdCAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgaWYgY3VycmVudCA8IGxvd2VzdCB0aGVuXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgY3VycmVudFxuXG4gICAgICAgICAgICAgICAgICAgICAgICBlbHNlXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgbG93ZXN0XG4gICAgICAgICAgICAgICAgICAgIClcbiAgICAgICAgICAgICAgICAgICAgdmFsXG4gICAgICAgICAgICAgICAgICAgIGFycmF5XG5cblxuey18IEZpbmQgdGhlIG1heGltdW0gZWxlbWVudCBpbiBhIG5vbi1lbXB0eSBsaXN0LlxuXG4gICAgbWF4aW11bSBbMywyLDFdID09IEp1c3QgM1xuICAgIG1heGltdW0gW10gICAgICA9PSBOb3RoaW5nXG5cbi19XG5tYXhpbXVtIDogQXJyYXkgY29tcGFyYWJsZSAtPiBNYXliZSBjb21wYXJhYmxlXG5tYXhpbXVtIGFycmF5ID1cbiAgICBjYXNlIGZpcnN0IGFycmF5IG9mXG4gICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgIE5vdGhpbmdcblxuICAgICAgICBKdXN0IHZhbCAtPlxuICAgICAgICAgICAgSnVzdCA8fFxuICAgICAgICAgICAgICAgIGZvbGRsXG4gICAgICAgICAgICAgICAgICAgIChcXGN1cnJlbnQgaGlnaGVzdCAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgaWYgY3VycmVudCA+IGhpZ2hlc3QgdGhlblxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGN1cnJlbnRcblxuICAgICAgICAgICAgICAgICAgICAgICAgZWxzZVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGhpZ2hlc3RcbiAgICAgICAgICAgICAgICAgICAgKVxuICAgICAgICAgICAgICAgICAgICB2YWxcbiAgICAgICAgICAgICAgICAgICAgYXJyYXlcblxuXG4tLSBNT0RJRllcblxuXG57LXwgUmVwbGFjZXMgdGhlIGVsZW1lbnQgYXQgdGhlIGdpdmVuIGluZGV4LCBvciByZXR1cm5zIHRoZSBhcnJheSB1bm1vZGlmaWVkIGlmIHRoZSBpbmRleCBpcyBvdXQgb2YgYm91bmRzLlxuUGFzc2luZyBhIG5lZ2F0aXZlIGluZGV4IG1lYW5zIHlvdSB3YW50IHRvIHJlcGxhY2UgYW4gZWxlbWVudCBjb3VudGluZyBiYWNrd2FyZHMgZnJvbSB0aGUgZW5kIG9mIHRoZSBhcnJheS5cblxuICAgIHNldCAxIDEwIFsgMSwgMiwgMyBdID09IFsgMSwgMTAsIDMgXVxuICAgIHNldCAxMCAxMCBbIDEsIDIsIDMgXSA9PSBbIDEsIDIsIDMgXVxuICAgIHNldCAtMSAxMCBbIDEsIDIsIDMgXSA9PSBbIDEsIDIsIDEwIF1cblxuLX1cbnNldCA6IEludCAtPiBhIC0+IEFycmF5IGEgLT4gQXJyYXkgYVxuc2V0ID1cbiAgICBHcmVuLktlcm5lbC5BcnJheS5zZXRcblxuXG57LXwgVXBkYXRlIGEgdmFsdWUgYXQgdGhlIGdpdmVuIGluZGV4IHVzaW5nIGEgZnVuY3Rpb24uIElmIHRoZSBpbmRleCBpcyBvdXQgb2YgYm91bmRzLCBub3RoaW5nIGhhcHBlbnMuXG5cbiAgICB1cGRhdGUgMSAoXFxuIC0+IG4gKyAxKSBbIDEsIDIsIDMgXSA9PSBbIDEsIDMsIDMgXVxuICAgIHVwZGF0ZSAxMCAoXFxuIC0+IG4gKyAxKSBbIDEsIDIsIDMgXSA9PSBbIDEsIDIsIDMgXVxuXG4tfVxudXBkYXRlIDogSW50IC0+IChhIC0+IGEpIC0+IEFycmF5IGEgLT4gQXJyYXkgYVxudXBkYXRlIGlkeCBmbiBhcnJheSA9XG4gICAgY2FzZSBnZXQgaWR4IGFycmF5IG9mXG4gICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgIGFycmF5XG5cbiAgICAgICAgSnVzdCB2YWwgLT5cbiAgICAgICAgICAgIHNldCBpZHggKGZuIHZhbCkgYXJyYXlcblxuXG57LXwgQWRkIGEgdmFsdWUgdG8gdGhlIHN0YXJ0IG9mIHRoZSBhcnJheS5cblxuICAgIHB1c2hGaXJzdCAxIFtdICAgICAgICAgID09IFsgMSBdXG4gICAgcHVzaEZpcnN0IDUgWyAxLCA0LCA5IF0gPT0gWyA1LCAxLCA0LCA5IF1cblxuLX1cbnB1c2hGaXJzdCA6IGEgLT4gQXJyYXkgYSAtPiBBcnJheSBhXG5wdXNoRmlyc3QgdmFsdWUgYXJyYXkgPVxuICAgIHByZXBlbmQgWyB2YWx1ZSBdIGFycmF5XG5cblxuey18IEFkZCBhIHZhbHVlIHRvIHRoZSBlbmQgb2YgdGhlIGFycmF5LlxuXG4gICAgcHVzaExhc3QgMSBbXSAgICAgICAgICA9PSBbIDEgXVxuICAgIHB1c2hMYXN0IDUgWyAxLCA0LCA5IF0gPT0gWyAxLCA0LCA5LCA1IF1cblxuLX1cbnB1c2hMYXN0IDogYSAtPiBBcnJheSBhIC0+IEFycmF5IGFcbnB1c2hMYXN0ID1cbiAgICBHcmVuLktlcm5lbC5BcnJheS5wdXNoXG5cblxuLS0gQ09NQklORVxuXG5cbnstfCBDb21iaW5lIHR3byBhcnJheXMgc28gdGhhdCB0aGUgZmlyc3QgYXJyYXkgYmVjb21lcyB0aGUgcHJlZml4LFxuYW5kIHRoZSBzZWNvbmQgYXJyYXkgYmVjb21lcyB0aGUgcG9zdGZpeCBvZiB0aGUgcmVzdWx0aW5nIGFycmF5LlxuXG4gICAgcHJlcGVuZCBbIDEsIDIsIDMgXSBbIDQsIDUsIDYgXSA9PSBbIDEsIDIsIDMsIDQsIDUsIDYgXSBcblxuWW91IGNhbiBhbHNvIHVzZSB0aGUgYCsrYCBvcGVyYXRvciBmb3IgdGhpcyBwdXJwb3NlLlxuXG4gICAgWyAxLCAyLCAzIF0gKysgWyA0LCA1LCA2IF0gPT0gWyAxLCAyLCAzLCA0LCA1LCA2IF1cbi19XG5wcmVwZW5kIDogQXJyYXkgYSAtPiBBcnJheSBhIC0+IEFycmF5IGFcbnByZXBlbmQgPVxuICAgIEdyZW4uS2VybmVsLkFycmF5LmFwcGVuZFxuXG5cbnstfCBDb21iaW5lIHR3byBhcnJheXMgc28gdGhhdCB0aGUgZmlyc3QgYXJyYXkgYmVjb21lcyB0aGUgcG9zdGZpeCxcbmFuZCB0aGUgc2Vjb25kIGFycmF5IGJlY29tZXMgdGhlIHByZWZpeCBvZiB0aGUgcmVzdWx0aW5nIGFycmF5LlxuXG4gICAgYXBwZW5kIFsgMSwgMiwgMyBdIFsgNCwgNSwgNiBdID09IFsgNCwgNSwgNiwgMSwgMiwgMyBdXG4gICAgXG4tfVxuYXBwZW5kIDogQXJyYXkgYSAtPiBBcnJheSBhIC0+IEFycmF5IGFcbmFwcGVuZCBmc3Qgc2Vjb25kID1cbiAgICBwcmVwZW5kIHNlY29uZCBmc3RcblxuXG57LXwgQ29tYmluZSBhIGJ1bmNoIG9mIGFycmF5cyBpbnRvIGEgc2luZ2xlIGFycmF5LlxuXG4gICAgZmxhdHRlbiBbIFsgMSBdLCBbIDIgXSwgWyA0LCA1IF0gXSA9PSBbIDEsIDIsIDQsIDUgXVxuXG4tfVxuZmxhdHRlbiA6IEFycmF5IChBcnJheSBhKSAtPiBBcnJheSBhXG5mbGF0dGVuID1cbiAgICBHcmVuLktlcm5lbC5BcnJheS5mbGF0XG5cblxuey18IE1hcCBhIGdpdmVuIGZ1bmN0aW9uIG9udG8gYW4gYXJyYXksIHRoZW4gZmxhdHRlbiB0aGUgcmVzdWx0aW5nIGFycmF5LlxuXG4gICAgZmxhdE1hcCBmIHhzID09IGZsYXR0ZW4gKG1hcCBmIHhzKVxuXG4tfVxuZmxhdE1hcCA6IChhIC0+IEFycmF5IGIpIC0+IEFycmF5IGEgLT4gQXJyYXkgYlxuZmxhdE1hcCA9XG4gICAgR3Jlbi5LZXJuZWwuQXJyYXkuZmxhdE1hcFxuXG5cbnstfCBQbGFjZXMgdGhlIGdpdmVuIHZhbHVlIGJldHdlZW4gYWxsIG1lbWJlcnMgb2YgdGhlIGdpdmVuIGxpc3QuXG5cbiAgICBpbnRlcnNwZXJzZSBcIm9uXCIgWyBcInR1cnRsZXNcIiwgXCJ0dXJ0bGVzXCIsIFwidHVydGxlc1wiXSA9PSBbIFwidHVydGxlc1wiLCBcIm9uXCIsIFwidHVydGxlc1wiLCBcIm9uXCIsIFwidHVydGxlc1wiXVxuXG4tfVxuaW50ZXJzcGVyc2UgOiBhIC0+IEFycmF5IGEgLT4gQXJyYXkgYVxuaW50ZXJzcGVyc2Ugc2VwIHhzID1cbiAgICBjYXNlIHBvcEZpcnN0IHhzIG9mXG4gICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgIFtdXG5cbiAgICAgICAgSnVzdCB7IGZpcnN0ID0gaGVhZCwgcmVzdCA9IHRhaWwgfSAtPlxuICAgICAgICAgICAgcHVzaEZpcnN0IGhlYWQgPHwgZmxhdE1hcCAoXFx2YWwgLT4gWyBzZXAsIHZhbCBdKSB0YWlsXG5cblxuey18IENvbWJpbmUgdHdvIGFycmF5cywgY29tYmluaW5nIHRoZW0gd2l0aCB0aGUgZ2l2ZW4gZnVuY3Rpb24uXG5JZiBvbmUgYXJyYXkgaXMgbG9uZ2VyLCB0aGUgZXh0cmEgZWxlbWVudHMgYXJlIGRyb3BwZWQuXG5cbiAgICBtYXAyIChcXHggeSAtPiB7IHggPSB4LCB5ID0geSB9KSBbIDEgXSBbIDIgXSA9PSBbIHsgeCA9IDEsIHkgPSAyIH0gXVxuLX1cbm1hcDIgOiAoYSAtPiBiIC0+IHJlc3VsdCkgLT4gQXJyYXkgYSAtPiBBcnJheSBiIC0+IEFycmF5IHJlc3VsdFxubWFwMiA9XG4gICAgR3Jlbi5LZXJuZWwuQXJyYXkubWFwMlxuXG5cbnstfCBDb21iaW5lIHRocmVlIGFycmF5cywgY29tYmluaW5nIHRoZW0gd2l0aCB0aGUgZ2l2ZW4gZnVuY3Rpb24uXG5JZiBvbmUgYXJyYXkgaXMgbG9uZ2VyLCB0aGUgZXh0cmEgZWxlbWVudHMgYXJlIGRyb3BwZWQuXG5cbiAgICBtYXAzIChcXHggeSB6IC0+IHsgeCA9IHgsIHkgPSB5LCB6ID0geiB9KSBbIDEgXSBbIDIgXSBbIDMgXSA9PSBbIHsgeCA9IDEsIHkgPSAyLCB6ID0gMyB9IF1cbi19XG5tYXAzIDogKGEgLT4gYiAtPiBjIC0+IHJlc3VsdCkgLT4gQXJyYXkgYSAtPiBBcnJheSBiIC0+IEFycmF5IGMgLT4gQXJyYXkgcmVzdWx0XG5tYXAzID1cbiAgICBHcmVuLktlcm5lbC5BcnJheS5tYXAzXG5cblxuLS0gREVDT05TVFJVQ1RcblxuXG57LXwgUmV0cmlldmUgdGhlIGZpcnN0IGVsZW1lbnQgb2YgdGhlIGFycmF5LCBpZiBpdCBleGlzdHMuXG5cbiAgICBmaXJzdCBbIDEsIDIsIDMgXSA9PSBKdXN0IDFcblxuLX1cbmZpcnN0IDogQXJyYXkgYSAtPiBNYXliZSBhXG5maXJzdCBhcnJheSA9XG4gICAgZ2V0IDAgYXJyYXlcblxuXG57LXwgUmV0cmlldmUgdGhlIGxhc3QgZWxlbWVudCBvZiB0aGUgYXJyYXksIGlmIGl0IGV4aXN0cy5cblxuICAgIGxhc3QgWyAxLCAyLCAzIF0gPT0gSnVzdCAzXG5cbi19XG5sYXN0IDogQXJyYXkgYSAtPiBNYXliZSBhXG5sYXN0IGFycmF5ID1cbiAgICBnZXQgLTEgYXJyYXlcblxuXG57LXwgR2V0IGEgc3ViIHNlY3Rpb24gb2YgYW4gYXJyYXk6IGAoc2xpY2Ugc3RhcnQgZW5kIGFycmF5KWAuXG5cblRoZSBgc3RhcnRgIGlzIGEgemVyby1iYXNlZCBpbmRleCB3aGVyZSB3ZSB3aWxsIHN0YXJ0IG91ciBzbGljZS5cblRoZSBgZW5kYCBpcyBhIHplcm8tYmFzZWQgaW5kZXggdGhhdCBpbmRpY2F0ZXMgdGhlIGVuZCBvZiB0aGUgc2xpY2UuXG5UaGUgc2xpY2UgZXh0cmFjdHMgdXAgdG8sIGJ1dCBubyBpbmNsdWRpbmcsIHRoZSBgZW5kYC5cblxuQm90aCBgc3RhcnRgIGFuZCBgZW5kYCBjYW4gYmUgbmVnYXRpdmUsIGluZGljYXRpbmcgYW4gb2Zmc2V0IGZyb20gdGhlIGVuZFxub2YgdGhlIGFycmF5LiBSZW1vdmluZyB0aGUgbGFzdCBlbGVtZW50IG9mIHRoZSBhcnJheSBjYW4gYmUgZXhwcmVzc2VkIGFzOlxuXG4gICAgYHNsaWNlIDAgLTEgYXJyYC5cblxuSW4gdGhlIGNhc2Ugb2YgYW4gaW1wb3NzaWJsZSBzbGljZSwgdGhlIGVtcHR5IGFycmF5IGlzIHJldHVybmVkLlxuXG4tfVxuc2xpY2UgOiBJbnQgLT4gSW50IC0+IEFycmF5IGEgLT4gQXJyYXkgYVxuc2xpY2UgPVxuICAgIEdyZW4uS2VybmVsLkFycmF5LnNsaWNlXG5cblxuey18IFJlbW92ZSB0aGUgZmlyc3QgYG5gIGVsZW1lbnRzIG9mIHRoZSBhcnJheS5cblxuICAgIGRyb3BGaXJzdCA1IFsgMSBdID09IFtdXG4gICAgZHJvcEZpcnN0IDEgWyAxLCAyLCAzIF0gPT0gWyAyLCAzIF1cblxuLX1cbmRyb3BGaXJzdCA6IEludCAtPiBBcnJheSBhIC0+IEFycmF5IGFcbmRyb3BGaXJzdCBuIGFycmF5ID1cbiAgICBzbGljZSBuIChsZW5ndGggYXJyYXkpIGFycmF5XG5cblxuey18IFJlbW92ZSB0aGUgbGFzdCBgbmAgZWxlbWVudHMgb2YgdGhlIGFycmF5LlxuXG4gICAgZHJvcExhc3QgMSBbIDEsIDIsIDMgXSA9PSBbIDEsIDIgXVxuXG4tfVxuZHJvcExhc3QgOiBJbnQgLT4gQXJyYXkgYSAtPiBBcnJheSBhXG5kcm9wTGFzdCBuIGFycmF5ID1cbiAgICBzbGljZSAwIChsZW5ndGggYXJyYXkgLSBuKSBhcnJheVxuXG5cbnstfCBUYWtlIHRoZSBmaXJzdCBgbmAgZWxlbWVudHMgZnJvbSB0aGUgYXJyYXkuXG5cbiAgICB0YWtlRmlyc3QgMiBbIDEsIDIsIDMgXSA9PSBbIDEsIDIgXVxuXG4tfVxudGFrZUZpcnN0IDogSW50IC0+IEFycmF5IGEgLT4gQXJyYXkgYVxudGFrZUZpcnN0IG4gYXJyYXkgPVxuICAgIHNsaWNlIDAgbiBhcnJheVxuXG5cbnstfCBUYWtlIHRoZSBsYXN0IGBuYCBlbGVtZW50cyBmcm9tIHRoZSBhcnJheS5cblxuICAgIHRha2VMYXN0IDIgWyAxLCAyLCAzIF0gPT0gWyAyLCAzIF1cblxuLX1cbnRha2VMYXN0IDogSW50IC0+IEFycmF5IGEgLT4gQXJyYXkgYVxudGFrZUxhc3QgbiBhcnJheSA9XG4gICAgbGV0XG4gICAgICAgIGxlbiA9XG4gICAgICAgICAgICBsZW5ndGggYXJyYXlcbiAgICBpblxuICAgIHNsaWNlIChsZW4gLSBuKSBsZW4gYXJyYXlcblxuXG57LXwgU3BsaXQgYW4gYXJyYXkgaW50byBpdHMgZmlyc3QgZWxlbWVudCwgYW5kIGl0cyByZW1haW5pbmcgZWxlbWVudHMsIGlmIHBvc3NpYmxlLlxuXG4gICAgcG9wRmlyc3QgWyAxLCAyLCAzIF0gPT0gSnVzdCB7IGZpcnN0ID0gMSwgcmVzdCA9IFsgMiwgMyBdIH1cblxuLX1cbnBvcEZpcnN0IDogQXJyYXkgYSAtPiBNYXliZSB7IGZpcnN0IDogYSwgcmVzdCA6IEFycmF5IGEgfVxucG9wRmlyc3QgYXJyYXkgPVxuICAgIGNhc2UgZmlyc3QgYXJyYXkgb2ZcbiAgICAgICAgSnVzdCB2YWx1ZSAtPlxuICAgICAgICAgICAgSnVzdFxuICAgICAgICAgICAgICAgIHsgZmlyc3QgPSB2YWx1ZVxuICAgICAgICAgICAgICAgICwgcmVzdCA9IGRyb3BGaXJzdCAxIGFycmF5XG4gICAgICAgICAgICAgICAgfVxuXG4gICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgIE5vdGhpbmdcblxuXG57LXwgU3BsaXQgYW4gYXJyYXkgaW50byBpdHMgbGFzdCBlbGVtZW50LCBhbmQgaXRzIHJlbWFpbmluZyBlbGVtZW50cywgaWYgcG9zc2libGUuXG5cbiAgICBwb3BGaXJzdCBbIDEsIDIsIDMgXSA9PSBKdXN0IHsgbGFzdCA9IDMsIGluaXRpYWwgPSBbIDEsIDIgXSB9XG5cbi19XG5wb3BMYXN0IDogQXJyYXkgYSAtPiBNYXliZSB7IGxhc3QgOiBhLCBpbml0aWFsIDogQXJyYXkgYSB9XG5wb3BMYXN0IGFycmF5ID1cbiAgICBjYXNlIGxhc3QgYXJyYXkgb2ZcbiAgICAgICAgSnVzdCB2YWx1ZSAtPlxuICAgICAgICAgICAgSnVzdFxuICAgICAgICAgICAgICAgIHsgbGFzdCA9IHZhbHVlXG4gICAgICAgICAgICAgICAgLCBpbml0aWFsID0gZHJvcExhc3QgMSBhcnJheVxuICAgICAgICAgICAgICAgIH1cblxuICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICBOb3RoaW5nXG5cblxuey18IERpdmlkZSBlbGVtZW50cyBpbnRvIHR3byBhcnJheXMgYmFzZWQgb24gdGhlIHJlc3VsdCBvZiBhIGJvb2xlYW4gdGVzdC5cblxuICAgIHBhcnRpdGlvbiAoXFx4IC0+IHggPCAzKSBbIDAsIDEsIDIsIDMsIDQsIDUgXSA9PSB7IHRydWVzID0gWyAwLCAxLCAyIF0sIGZhbHNlcyA9IFsgMywgNCwgNSBdIH1cblxuLX1cbnBhcnRpdGlvbiA6IChhIC0+IEJvb2wpIC0+IEFycmF5IGEgLT4geyB0cnVlcyA6IEFycmF5IGEsIGZhbHNlcyA6IEFycmF5IGEgfVxucGFydGl0aW9uIGZuIGFycmF5ID1cbiAgICBmb2xkbFxuICAgICAgICAoXFx2YWwgeyB0cnVlcywgZmFsc2VzIH0gLT5cbiAgICAgICAgICAgIGlmIGZuIHZhbCB0aGVuXG4gICAgICAgICAgICAgICAgeyB0cnVlcyA9IHB1c2hMYXN0IHZhbCB0cnVlc1xuICAgICAgICAgICAgICAgICwgZmFsc2VzID0gZmFsc2VzXG4gICAgICAgICAgICAgICAgfVxuXG4gICAgICAgICAgICBlbHNlXG4gICAgICAgICAgICAgICAgeyB0cnVlcyA9IHRydWVzXG4gICAgICAgICAgICAgICAgLCBmYWxzZXMgPSBwdXNoTGFzdCB2YWwgZmFsc2VzXG4gICAgICAgICAgICAgICAgfVxuICAgICAgICApXG4gICAgICAgIHsgdHJ1ZXMgPSBlbXB0eSwgZmFsc2VzID0gZW1wdHkgfVxuICAgICAgICBhcnJheVxuXG5cbi0tIFNPUlRcblxuXG57LXwgU29ydCB2YWx1ZXMgZnJvbSBsb3dlc3QgdG8gaGlnaGVzdFxuXG4gICAgc29ydCBbIDMsIDEsIDUgXSA9PSBbIDEsIDMsIDUgXVxuXG4tfVxuc29ydCA6IEFycmF5IGNvbXBhcmFibGUgLT4gQXJyYXkgY29tcGFyYWJsZVxuc29ydCA9XG4gICAgR3Jlbi5LZXJuZWwuQXJyYXkuc29ydFxuXG5cbnstfCBTb3J0IHZhbHVlcyBieSBhIGRlcml2ZWQgcHJvcGVydHkuXG5cbiAgICBzb3J0QnkgU3RyaW5nLmxlbmd0aCBbIFwibW91c2VcIiwgXCJjYXRcIiBdID09IFsgXCJjYXRcIiwgXCJtb3VzZVwiIF1cblxuLX1cbnNvcnRCeSA6IChhIC0+IGNvbXBhcmFibGUpIC0+IEFycmF5IGEgLT4gQXJyYXkgYVxuc29ydEJ5ID1cbiAgICBHcmVuLktlcm5lbC5BcnJheS5zb3J0QnlcblxuXG57LXwgU29ydCB2YWx1ZXMgd2l0aCBhIGN1c3RvbSBjb21wYXJpc29uIGZ1bmN0aW9uLlxuXG4gICAgc29ydFdpdGggZmxpcHBlZENvbXBhcmlzb24gWzEsMiwzLDQsNV0gPT0gWzUsNCwzLDIsMV1cblxuICAgIGZsaXBwZWRDb21wYXJpc29uIGEgYiA9XG4gICAgICAgIGNhc2UgY29tcGFyZSBhIGIgb2ZcbiAgICAgICAgICBMVCAtPiBHVFxuICAgICAgICAgIEVRIC0+IEVRXG4gICAgICAgICAgR1QgLT4gTFRcblxuVGhpcyBpcyBhbHNvIHRoZSBtb3N0IGdlbmVyYWwgc29ydCBmdW5jdGlvbiwgYWxsb3dpbmcgeW91IHRvIGRlZmluZSBhbnkgb3RoZXI6IGBzb3J0ID09IHNvcnRXaXRoIGNvbXBhcmVgXG5cbi19XG5zb3J0V2l0aCA6IChhIC0+IGEgLT4gT3JkZXIpIC0+IEFycmF5IGEgLT4gQXJyYXkgYVxuc29ydFdpdGggPVxuICAgIEdyZW4uS2VybmVsLkFycmF5LnNvcnRXaXRoXG5cbiIsCiAgICAgICAgIm1vZHVsZSBTZXQgZXhwb3NpbmdcbiAgICAoIFNldFxuICAgICwgZW1wdHksIHNpbmdsZXRvbiwgc2V0LCByZW1vdmVcbiAgICAsIGlzRW1wdHksIG1lbWJlciwgY291bnQsIGZpcnN0LCBsYXN0LCBmaW5kRmlyc3QsIGZpbmRMYXN0LCBhbnksIGFsbFxuICAgICwgdW5pb24sIGludGVyc2VjdCwgZGlmZlxuICAgICwgdG9BcnJheSwgZnJvbUFycmF5XG4gICAgLCBtYXAsIGZvbGRsLCBmb2xkciwgZmlsdGVyLCBmaWx0ZXJNYXAsIHBhcnRpdGlvblxuICAgIClcblxuey18IEEgc2V0IG9mIHVuaXF1ZSB2YWx1ZXMuIFRoZSB2YWx1ZXMgY2FuIGJlIGFueSBjb21wYXJhYmxlIHR5cGUuIFRoaXNcbmluY2x1ZGVzIGBJbnRgLCBgRmxvYXRgLCBgVGltZWAsIGBDaGFyYCwgYFN0cmluZ2AsIGFuZCB0dXBsZXMgb3IgbGlzdHNcbm9mIGNvbXBhcmFibGUgdHlwZXMuXG5cblNldCwgcmVtb3ZlLCBhbmQgcXVlcnkgb3BlcmF0aW9ucyBhbGwgdGFrZSBfTyhsb2cgbilfIHRpbWUuXG5cblxuQGRvY3MgU2V0XG5cblxuQGRvY3MgZW1wdHksIHNpbmdsZXRvbiwgc2V0LCByZW1vdmVcblxuXG4jIyBRdWVyeVxuXG5AZG9jcyBpc0VtcHR5LCBtZW1iZXIsIGNvdW50LCBmaXJzdCwgbGFzdCwgZmluZEZpcnN0LCBmaW5kTGFzdCwgYW55LCBhbGxcblxuXG4jIyBDb21iaW5lXG5cbkBkb2NzIHVuaW9uLCBpbnRlcnNlY3QsIGRpZmZcblxuXG4jIyBBcnJheXNcblxuQGRvY3MgdG9BcnJheSwgZnJvbUFycmF5XG5cblxuIyMgVHJhbnNmb3JtXG5cbkBkb2NzIG1hcCwgZm9sZGwsIGZvbGRyLCBmaWx0ZXIsIGZpbHRlck1hcCwgcGFydGl0aW9uXG5cbi19XG5cbmltcG9ydCBBcnJheSBleHBvc2luZyAoQXJyYXkpXG5pbXBvcnQgQmFzaWNzIGV4cG9zaW5nIChCb29sLCBJbnQpXG5pbXBvcnQgRGljdFxuaW1wb3J0IE1heWJlIGV4cG9zaW5nIChNYXliZSguLikpXG5cblxuey18IFJlcHJlc2VudHMgYSBzZXQgb2YgdW5pcXVlIHZhbHVlcy4gU28gYChTZXQgSW50KWAgaXMgYSBzZXQgb2YgaW50ZWdlcnMgYW5kXG5gKFNldCBTdHJpbmcpYCBpcyBhIHNldCBvZiBzdHJpbmdzLlxuLX1cbnR5cGUgU2V0IHRcbiAgICA9IFNldF9ncmVuX2J1aWx0aW4gKERpY3QuRGljdCB0IHt9KVxuXG5cbnstfCBDcmVhdGUgYW4gZW1wdHkgc2V0LlxuLX1cbmVtcHR5IDogU2V0IGFcbmVtcHR5ID1cbiAgICBTZXRfZ3Jlbl9idWlsdGluIERpY3QuZW1wdHlcblxuXG57LXwgQ3JlYXRlIGEgc2V0IHdpdGggb25lIHZhbHVlLlxuLX1cbnNpbmdsZXRvbiA6IGNvbXBhcmFibGUgLT4gU2V0IGNvbXBhcmFibGVcbnNpbmdsZXRvbiBrZXkgPVxuICAgIFNldF9ncmVuX2J1aWx0aW4gKERpY3Quc2luZ2xldG9uIGtleSB7fSlcblxuXG57LXwgU2V0IGEgdmFsdWUgaW50byBhIHNldC5cbi19XG5zZXQgOiBjb21wYXJhYmxlIC0+IFNldCBjb21wYXJhYmxlIC0+IFNldCBjb21wYXJhYmxlXG5zZXQga2V5IChTZXRfZ3Jlbl9idWlsdGluIGRpY3QpID1cbiAgICBTZXRfZ3Jlbl9idWlsdGluIChEaWN0LnNldCBrZXkge30gZGljdClcblxuXG57LXwgUmVtb3ZlIGEgdmFsdWUgZnJvbSBhIHNldC4gSWYgdGhlIHZhbHVlIGlzIG5vdCBmb3VuZCwgbm8gY2hhbmdlcyBhcmUgbWFkZS5cbi19XG5yZW1vdmUgOiBjb21wYXJhYmxlIC0+IFNldCBjb21wYXJhYmxlIC0+IFNldCBjb21wYXJhYmxlXG5yZW1vdmUga2V5IChTZXRfZ3Jlbl9idWlsdGluIGRpY3QpID1cbiAgICBTZXRfZ3Jlbl9idWlsdGluIChEaWN0LnJlbW92ZSBrZXkgZGljdClcblxuXG57LXwgRGV0ZXJtaW5lIGlmIGEgc2V0IGlzIGVtcHR5LlxuLX1cbmlzRW1wdHkgOiBTZXQgYSAtPiBCb29sXG5pc0VtcHR5IChTZXRfZ3Jlbl9idWlsdGluIGRpY3QpID1cbiAgICBEaWN0LmlzRW1wdHkgZGljdFxuXG5cbnstfCBEZXRlcm1pbmUgaWYgYSB2YWx1ZSBpcyBpbiBhIHNldC5cbi19XG5tZW1iZXIgOiBjb21wYXJhYmxlIC0+IFNldCBjb21wYXJhYmxlIC0+IEJvb2xcbm1lbWJlciBrZXkgKFNldF9ncmVuX2J1aWx0aW4gZGljdCkgPVxuICAgIERpY3QubWVtYmVyIGtleSBkaWN0XG5cblxuey18IERldGVybWluZSB0aGUgbnVtYmVyIG9mIGVsZW1lbnRzIGluIGEgc2V0LlxuLX1cbmNvdW50IDogU2V0IGEgLT4gSW50XG5jb3VudCAoU2V0X2dyZW5fYnVpbHRpbiBkaWN0KSA9XG4gICAgRGljdC5jb3VudCBkaWN0XG5cblxuey18IEdldCB0aGUgZmlyc3QgZWxlbWVudCBvZiB0aGUgc2V0LlxuLX1cbmZpcnN0IDogU2V0IGEgLT4gTWF5YmUgYVxuZmlyc3QgKFNldF9ncmVuX2J1aWx0aW4gZGljdCkgPVxuICAgIE1heWJlLm1hcCAua2V5IChEaWN0LmZpcnN0IGRpY3QpXG5cblxuey18IEdldCB0aGUgbGFzdCBlbGVtZW50IG9mIHRoZSBzZXQuXG4tfVxubGFzdCA6IFNldCBhIC0+IE1heWJlIGFcbmxhc3QgKFNldF9ncmVuX2J1aWx0aW4gZGljdCkgPVxuICAgIE1heWJlLm1hcCAua2V5IChEaWN0Lmxhc3QgZGljdClcblxuXG57LXwgRmluZCB0aGUgZmlyc3QgdmFsdWUgdGhhdCBwYXNzZXMgdGhlIHRlc3QuXG4tfVxuZmluZEZpcnN0IDogKGEgLT4gQm9vbCkgLT4gU2V0IGEgLT4gTWF5YmUgYVxuZmluZEZpcnN0IGZuIChTZXRfZ3Jlbl9idWlsdGluIGRpY3QpID1cbiAgICBNYXliZS5tYXAgLmtleSAoRGljdC5maW5kRmlyc3QgKFxca2V5IF8gLT4gZm4ga2V5KSBkaWN0KVxuXG5cbnstfCBGaW5kIHRoZSBsYXN0IHZhbHVlIHRoYXQgcGFzc2VzIHRoZSB0ZXN0LlxuLX1cbmZpbmRMYXN0IDogKGEgLT4gQm9vbCkgLT4gU2V0IGEgLT4gTWF5YmUgYVxuZmluZExhc3QgZm4gKFNldF9ncmVuX2J1aWx0aW4gZGljdCkgPVxuICAgIE1heWJlLm1hcCAua2V5IChEaWN0LmZpbmRMYXN0IChcXGtleSBfIC0+IGZuIGtleSkgZGljdClcblxuXG57LXwgQ2hlY2tzIGlmIGFueSB2YWx1ZSBpbiB0aGUgc2V0IHBhc3NlcyB0aGUgdGVzdC5cbi19XG5hbnkgOiAoYSAtPiBCb29sKSAtPiBTZXQgYSAtPiBCb29sXG5hbnkgZm4gKFNldF9ncmVuX2J1aWx0aW4gZGljdCkgPVxuICAgIERpY3QuYW55IChcXGtleSBfIC0+IGZuIGtleSkgZGljdFxuXG5cbnstfCBDaGVja3MgaWYgYWxsIHZhbHVlcyBpbiB0aGUgc2V0IHBhc3NlcyB0aGUgdGVzdC5cbi19XG5hbGwgOiAoYSAtPiBCb29sKSAtPiBTZXQgYSAtPiBCb29sXG5hbGwgZm4gKFNldF9ncmVuX2J1aWx0aW4gZGljdCkgPVxuICAgIERpY3QuYWxsIChcXGtleSBfIC0+IGZuIGtleSkgZGljdFxuXG5cbnstfCBHZXQgdGhlIHVuaW9uIG9mIHR3byBzZXRzLiBLZWVwIGFsbCB2YWx1ZXMuXG4tfVxudW5pb24gOiBTZXQgY29tcGFyYWJsZSAtPiBTZXQgY29tcGFyYWJsZSAtPiBTZXQgY29tcGFyYWJsZVxudW5pb24gKFNldF9ncmVuX2J1aWx0aW4gZGljdDEpIChTZXRfZ3Jlbl9idWlsdGluIGRpY3QyKSA9XG4gICAgU2V0X2dyZW5fYnVpbHRpbiAoRGljdC51bmlvbiBkaWN0MSBkaWN0MilcblxuXG57LXwgR2V0IHRoZSBpbnRlcnNlY3Rpb24gb2YgdHdvIHNldHMuIEtlZXBzIHZhbHVlcyB0aGF0IGFwcGVhciBpbiBib3RoIHNldHMuXG4tfVxuaW50ZXJzZWN0IDogU2V0IGNvbXBhcmFibGUgLT4gU2V0IGNvbXBhcmFibGUgLT4gU2V0IGNvbXBhcmFibGVcbmludGVyc2VjdCAoU2V0X2dyZW5fYnVpbHRpbiBkaWN0MSkgKFNldF9ncmVuX2J1aWx0aW4gZGljdDIpID1cbiAgICBTZXRfZ3Jlbl9idWlsdGluIChEaWN0LmludGVyc2VjdCBkaWN0MSBkaWN0MilcblxuXG57LXwgR2V0IHRoZSBkaWZmZXJlbmNlIGJldHdlZW4gdGhlIGZpcnN0IHNldCBhbmQgdGhlIHNlY29uZC4gS2VlcHMgdmFsdWVzXG50aGF0IGRvIG5vdCBhcHBlYXIgaW4gdGhlIHNlY29uZCBzZXQuXG4tfVxuZGlmZiA6IFNldCBjb21wYXJhYmxlIC0+IFNldCBjb21wYXJhYmxlIC0+IFNldCBjb21wYXJhYmxlXG5kaWZmIChTZXRfZ3Jlbl9idWlsdGluIGRpY3QxKSAoU2V0X2dyZW5fYnVpbHRpbiBkaWN0MikgPVxuICAgIFNldF9ncmVuX2J1aWx0aW4gKERpY3QuZGlmZiBkaWN0MSBkaWN0MilcblxuXG57LXwgQ29udmVydCBhIHNldCBpbnRvIGEgbGlzdCwgc29ydGVkIGZyb20gbG93ZXN0IHRvIGhpZ2hlc3QuXG4tfVxudG9BcnJheSA6IFNldCBhIC0+IEFycmF5IGFcbnRvQXJyYXkgKFNldF9ncmVuX2J1aWx0aW4gZGljdCkgPVxuICAgIERpY3Qua2V5cyBkaWN0XG5cblxuey18IENvbnZlcnQgYSBsaXN0IGludG8gYSBzZXQsIHJlbW92aW5nIGFueSBkdXBsaWNhdGVzLlxuLX1cbmZyb21BcnJheSA6IEFycmF5IGNvbXBhcmFibGUgLT4gU2V0IGNvbXBhcmFibGVcbmZyb21BcnJheSBsaXN0ID1cbiAgICBBcnJheS5mb2xkbCBzZXQgZW1wdHkgbGlzdFxuXG5cbnstfCBGb2xkIG92ZXIgdGhlIHZhbHVlcyBpbiBhIHNldCwgaW4gb3JkZXIgZnJvbSBsb3dlc3QgdG8gaGlnaGVzdC5cbi19XG5mb2xkbCA6IChhIC0+IGIgLT4gYikgLT4gYiAtPiBTZXQgYSAtPiBiXG5mb2xkbCBmdW5jIGluaXRpYWxTdGF0ZSAoU2V0X2dyZW5fYnVpbHRpbiBkaWN0KSA9XG4gICAgRGljdC5mb2xkbCAoXFxrZXkgXyBzdGF0ZSAtPiBmdW5jIGtleSBzdGF0ZSkgaW5pdGlhbFN0YXRlIGRpY3RcblxuXG57LXwgRm9sZCBvdmVyIHRoZSB2YWx1ZXMgaW4gYSBzZXQsIGluIG9yZGVyIGZyb20gaGlnaGVzdCB0byBsb3dlc3QuXG4tfVxuZm9sZHIgOiAoYSAtPiBiIC0+IGIpIC0+IGIgLT4gU2V0IGEgLT4gYlxuZm9sZHIgZnVuYyBpbml0aWFsU3RhdGUgKFNldF9ncmVuX2J1aWx0aW4gZGljdCkgPVxuICAgIERpY3QuZm9sZHIgKFxca2V5IF8gc3RhdGUgLT4gZnVuYyBrZXkgc3RhdGUpIGluaXRpYWxTdGF0ZSBkaWN0XG5cblxuey18IE1hcCBhIGZ1bmN0aW9uIG9udG8gYSBzZXQsIGNyZWF0aW5nIGEgbmV3IHNldCB3aXRoIG5vIGR1cGxpY2F0ZXMuXG4tfVxubWFwIDogKGNvbXBhcmFibGUgLT4gY29tcGFyYWJsZTIpIC0+IFNldCBjb21wYXJhYmxlIC0+IFNldCBjb21wYXJhYmxlMlxubWFwIGZ1bmMgY29sbCA9XG4gICAgZm9sZGwgKFxceCB4cyAtPiBzZXQgKGZ1bmMgeCkgeHMpIGVtcHR5IGNvbGxcblxuXG57LXwgT25seSBrZWVwIGVsZW1lbnRzIHRoYXQgcGFzcyB0aGUgZ2l2ZW4gdGVzdC5cblxuICAgIGltcG9ydCBTZXQgZXhwb3NpbmcgKFNldClcblxuICAgIG51bWJlcnMgOiBTZXQgSW50XG4gICAgbnVtYmVycyA9XG4gICAgICAgIFNldC5mcm9tQXJyYXkgWyAtMiwgLTEsIDAsIDEsIDIgXVxuXG4gICAgcG9zaXRpdmVzIDogU2V0IEludFxuICAgIHBvc2l0aXZlcyA9XG4gICAgICAgIFNldC5maWx0ZXIgKFxceCAtPiB4ID4gMCkgbnVtYmVyc1xuXG4gICAgLS0gcG9zaXRpdmVzID09IFNldC5mcm9tQXJyYXkgWzEsMl1cblxuLX1cbmZpbHRlciA6IChjb21wYXJhYmxlIC0+IEJvb2wpIC0+IFNldCBjb21wYXJhYmxlIC0+IFNldCBjb21wYXJhYmxlXG5maWx0ZXIgaXNHb29kIChTZXRfZ3Jlbl9idWlsdGluIGRpY3QpID1cbiAgICBTZXRfZ3Jlbl9idWlsdGluIChEaWN0LmZpbHRlciAoXFxrZXkgXyAtPiBpc0dvb2Qga2V5KSBkaWN0KVxuXG5cbnstfCBGaWx0ZXIgb3V0IHVud2FudGVkIHJlc3VsdHMgb2YgYSBtYXAgb3BlcmF0aW9uLlxuICAgIFxuICAgIGltcG9ydCBTZXRcblxuICAgIHN0cmluZ3MgOiBTZXQgU3RyaW5nXG4gICAgc3RyaW5ncyA9XG4gICAgICAgIFNldC5mcm9tQXJyYXkgWyBcIjNcIiwgXCJub3QgYSBudW1iZXJcIiwgXCItNVwiIF1cblxuICAgIG51bWJlcnMgOiBTZXQgSW50XG4gICAgbnVtYmVycyA9XG4gICAgICAgIFNldC5maWx0ZXJNYXAgU3RyaW5nLnRvSW50IHN0cmluZ3NcblxuICAgIC0tIG51bWJlcnMgPT0gU2V0LmZyb21BcnJheSBbIDMsIC01IF1cbi19XG5maWx0ZXJNYXAgOiAoY29tcGFyYWJsZSAtPiBNYXliZSBjb21wYXJhYmxlMikgLT4gU2V0IGNvbXBhcmFibGUgLT4gU2V0IGNvbXBhcmFibGUyXG5maWx0ZXJNYXAgdG9NYXliZSBjb2xsID1cbiAgICBmb2xkbFxuICAgICAgICAoXFxvbGQgbmV3cyAtPlxuICAgICAgICAgICAgY2FzZSB0b01heWJlIG9sZCBvZlxuICAgICAgICAgICAgICAgIEp1c3QgbmV3IC0+XG4gICAgICAgICAgICAgICAgICAgIHNldCBuZXcgbmV3c1xuXG4gICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICBuZXdzXG4gICAgICAgIClcbiAgICAgICAgZW1wdHlcbiAgICAgICAgY29sbFxuXG5cbnstfCBDcmVhdGUgdHdvIG5ldyBzZXRzLiBUaGUgZmlyc3QgY29udGFpbnMgYWxsIHRoZSBlbGVtZW50cyB0aGF0IHBhc3NlZCB0aGVcbmdpdmVuIHRlc3QsIGFuZCB0aGUgc2Vjb25kIGNvbnRhaW5zIGFsbCB0aGUgZWxlbWVudHMgdGhhdCBkaWQgbm90LlxuLX1cbnBhcnRpdGlvbiA6IChjb21wYXJhYmxlIC0+IEJvb2wpIC0+IFNldCBjb21wYXJhYmxlIC0+IHsgdHJ1ZXMgOiBTZXQgY29tcGFyYWJsZSwgZmFsc2VzIDogU2V0IGNvbXBhcmFibGUgfVxucGFydGl0aW9uIGlzR29vZCAoU2V0X2dyZW5fYnVpbHRpbiBkaWN0KSA9XG4gICAgbGV0XG4gICAgICAgIHsgdHJ1ZXMsIGZhbHNlcyB9ID1cbiAgICAgICAgICAgIERpY3QucGFydGl0aW9uIChcXGtleSBfIC0+IGlzR29vZCBrZXkpIGRpY3RcbiAgICBpblxuICAgIHsgdHJ1ZXMgPSBTZXRfZ3Jlbl9idWlsdGluIHRydWVzXG4gICAgLCBmYWxzZXMgPSBTZXRfZ3Jlbl9idWlsdGluIGZhbHNlc1xuICAgIH1cbiIsCiAgICAgICAgIm1vZHVsZSBTdHJpbmcgZXhwb3NpbmdcbiAgICAoIFN0cmluZywgaXNFbXB0eSwgbGVuZ3RoLCByZXZlcnNlLCByZXBlYXQsIHJlcGxhY2VcbiAgICAsIHByZXBlbmQsIGFwcGVuZCwgc3BsaXQsIGpvaW4sIHdvcmRzLCBsaW5lc1xuICAgICwgc2xpY2UsIGxlZnQsIHJpZ2h0LCBkcm9wTGVmdCwgZHJvcFJpZ2h0XG4gICAgLCBjb250YWlucywgc3RhcnRzV2l0aCwgZW5kc1dpdGgsIGluZGljZXNcbiAgICAsIHRvSW50LCBmcm9tSW50XG4gICAgLCB0b0Zsb2F0LCBmcm9tRmxvYXRcbiAgICAsIGZyb21DaGFyLCBjb25zLCB1bmNvbnNcbiAgICAsIHRvQXJyYXksIGZyb21BcnJheVxuICAgICwgdG9VcHBlciwgdG9Mb3dlciwgcGFkLCBwYWRMZWZ0LCBwYWRSaWdodCwgdHJpbSwgdHJpbUxlZnQsIHRyaW1SaWdodFxuICAgICwgbWFwLCBmaWx0ZXIsIGZvbGRsLCBmb2xkciwgYW55LCBhbGxcbiAgICApXG5cbnstfCBBIGJ1aWx0LWluIHJlcHJlc2VudGF0aW9uIGZvciBlZmZpY2llbnQgc3RyaW5nIG1hbmlwdWxhdGlvbi4gU3RyaW5nIGxpdGVyYWxzXG5hcmUgZW5jbG9zZWQgaW4gYFwiZG91YmxlIHF1b3Rlc1wiYC5cblxuQGRvY3MgU3RyaW5nLCBpc0VtcHR5LCBsZW5ndGgsIHJldmVyc2UsIHJlcGVhdCwgcmVwbGFjZVxuXG5cbiMjIEJ1aWxkaW5nIGFuZCBTcGxpdHRpbmdcblxuQGRvY3MgcHJlcGVuZCwgYXBwZW5kLCBzcGxpdCwgam9pbiwgd29yZHMsIGxpbmVzXG5cblxuIyMgR2V0IFN1YnN0cmluZ3NcblxuQGRvY3Mgc2xpY2UsIGxlZnQsIHJpZ2h0LCBkcm9wTGVmdCwgZHJvcFJpZ2h0XG5cblxuIyMgQ2hlY2sgZm9yIFN1YnN0cmluZ3NcblxuQGRvY3MgY29udGFpbnMsIHN0YXJ0c1dpdGgsIGVuZHNXaXRoLCBpbmRpY2VzXG5cblxuIyMgSW50IENvbnZlcnNpb25zXG5cbkBkb2NzIHRvSW50LCBmcm9tSW50XG5cblxuIyMgRmxvYXQgQ29udmVyc2lvbnNcblxuQGRvY3MgdG9GbG9hdCwgZnJvbUZsb2F0XG5cblxuIyMgQ2hhciBDb252ZXJzaW9uc1xuXG5AZG9jcyBmcm9tQ2hhciwgY29ucywgdW5jb25zXG5cblxuIyMgQXJyYXkgQ29udmVyc2lvbnNcblxuQGRvY3MgdG9BcnJheSwgZnJvbUFycmF5XG5cblxuIyMgRm9ybWF0dGluZ1xuXG5Db3NtZXRpYyBvcGVyYXRpb25zIHN1Y2ggYXMgcGFkZGluZyB3aXRoIGV4dHJhIGNoYXJhY3RlcnMgb3IgdHJpbW1pbmcgd2hpdGVzcGFjZS5cblxuQGRvY3MgdG9VcHBlciwgdG9Mb3dlciwgcGFkLCBwYWRMZWZ0LCBwYWRSaWdodCwgdHJpbSwgdHJpbUxlZnQsIHRyaW1SaWdodFxuXG4jIyBIaWdoZXItT3JkZXIgRnVuY3Rpb25zXG5cbkBkb2NzIG1hcCwgZmlsdGVyLCBmb2xkbCwgZm9sZHIsIGFueSwgYWxsXG5cbi19XG5cbmltcG9ydCBBcnJheSBleHBvc2luZyAoQXJyYXkpXG5pbXBvcnQgQmFzaWNzIGV4cG9zaW5nICguLilcbmltcG9ydCBNYXRoIGV4cG9zaW5nIChmbG9vciwgY2VpbGluZylcbmltcG9ydCBCaXR3aXNlXG5pbXBvcnQgQ2hhciBleHBvc2luZyAoQ2hhcilcbmltcG9ydCBHcmVuLktlcm5lbC5TdHJpbmdcbmltcG9ydCBNYXliZSBleHBvc2luZyAoTWF5YmUpXG5pbXBvcnQgUmVzdWx0IGV4cG9zaW5nIChSZXN1bHQpXG5cblxuXG4tLSBTVFJJTkdTXG5cblxuey18IEEgYFN0cmluZ2AgaXMgYSBjaHVuayBvZiB0ZXh0OlxuXG4gICAgXCJIZWxsbyFcIlxuXG4gICAgXCJIb3cgYXJlIHlvdT9cIlxuXG4gICAgXCLwn5mI8J+ZifCfmYpcIlxuXG4gICAgLS0gc3RyaW5ncyB3aXRoIGVzY2FwZSBjaGFyYWN0ZXJzXG4gICAgXCJ0aGlzXFxuXFx0XFxcInRoYXRcXFwiXCJcblxuICAgIFwi8J+ZiPCfmYnwn5mKXCIgLS0gXCLwn5mI8J+ZifCfmYpcIlxuXG4gICAgLS0gbXVsdGlsaW5lIHN0cmluZ3NcbiAgICBcIlwiXCJUcmlwbGUgZG91YmxlIHF1b3RlcyBsZXQgeW91XG4gICAgY3JlYXRlIFwibXVsdGlsaW5lIHN0cmluZ3NcIiB3aGljaFxuICAgIGNhbiBoYXZlIHVuZXNjYXBlZCBxdW90ZXMgYW5kIG5ld2xpbmVzLlxuICAgIFwiXCJcIlxuXG5BIGBTdHJpbmdgIGNhbiByZXByZXNlbnQgYW55IHNlcXVlbmNlIG9mIFt1bmljb2RlIGNoYXJhY3RlcnNdW3VdLiBZb3UgY2FuIHVzZVxudGhlIHVuaWNvZGUgZXNjYXBlcyBmcm9tIGBcXHV7MDAwMH1gIHRvIGBcXHV7MTBGRkZGfWAgdG8gcmVwcmVzZW50IGNoYXJhY3RlcnNcbmJ5IHRoZWlyIGNvZGUgcG9pbnQuIFlvdSBjYW4gYWxzbyBpbmNsdWRlIHRoZSB1bmljb2RlIGNoYXJhY3RlcnMgZGlyZWN0bHkuXG5Vc2luZyB0aGUgZXNjYXBlcyBjYW4gYmUgYmV0dGVyIGlmIHlvdSBuZWVkIG9uZSBvZiB0aGUgbWFueSB3aGl0ZXNwYWNlXG5jaGFyYWN0ZXJzIHdpdGggZGlmZmVyZW50IHdpZHRocy5cblxuW3VdOiBodHRwczovL2VuLndpa2lwZWRpYS5vcmcvd2lraS9Vbmljb2RlXG5cbioqTm90ZToqKiBKYXZhU2NyaXB0IGxldHMgeW91IHVzZSBkb3VibGUgcXVvdGVzIGFuZCBzaW5nbGUgcXVvdGVzIGludGVyY2hhbmdhYmx5LlxuVGhpcyBpcyBub3QgdHJ1ZSBpbiBHcmVuLiBZb3UgbXVzdCB1c2UgZG91YmxlIHF1b3RlcyBmb3IgYSBgU3RyaW5nYCwgYW5kIHlvdSBtdXN0XG51c2Ugc2luZ2xlIHF1b3RlcyBmb3IgYSBbYENoYXJgXShDaGFyI0NoYXIpLlxuXG4tfVxudHlwZSBTdHJpbmdcbiAgICA9IFN0cmluZyAtLSBOT1RFOiBUaGUgY29tcGlsZXIgcHJvdmlkZXMgdGhlIHJlYWwgaW1wbGVtZW50YXRpb24uXG5cblxuey18IERldGVybWluZSBpZiBhIHN0cmluZyBpcyBlbXB0eS5cblxuICAgIGlzRW1wdHkgXCJcIiA9PSBUcnVlXG5cbiAgICBpc0VtcHR5IFwidGhlIHdvcmxkXCIgPT0gRmFsc2VcblxuLX1cbmlzRW1wdHkgOiBTdHJpbmcgLT4gQm9vbFxuaXNFbXB0eSBzdHJpbmcgPVxuICAgIHN0cmluZyA9PSBcIlwiXG5cblxuey18IEdldCB0aGUgbGVuZ3RoIG9mIGEgc3RyaW5nLlxuXG4gICAgbGVuZ3RoIFwiaW5udW1lcmFibGVcIiA9PSAxMVxuXG4gICAgbGVuZ3RoIFwiXCIgPT0gMFxuXG4tfVxubGVuZ3RoIDogU3RyaW5nIC0+IEludFxubGVuZ3RoID1cbiAgICBHcmVuLktlcm5lbC5TdHJpbmcubGVuZ3RoXG5cblxuey18IFJldmVyc2UgYSBzdHJpbmcuXG5cbiAgICByZXZlcnNlIFwic3RyZXNzZWRcIiA9PSBcImRlc3NlcnRzXCJcblxuLX1cbnJldmVyc2UgOiBTdHJpbmcgLT4gU3RyaW5nXG5yZXZlcnNlID1cbiAgICBHcmVuLktlcm5lbC5TdHJpbmcucmV2ZXJzZVxuXG5cbnstfCBSZXBlYXQgYSBzdHJpbmcgX25fIHRpbWVzLlxuXG4gICAgcmVwZWF0IDMgXCJoYVwiID09IFwiaGFoYWhhXCJcblxuLX1cbnJlcGVhdCA6IEludCAtPiBTdHJpbmcgLT4gU3RyaW5nXG5yZXBlYXQgbiBjaHVuayA9XG4gICAgcmVwZWF0SGVscCBuIGNodW5rIFwiXCJcblxuXG5yZXBlYXRIZWxwIDogSW50IC0+IFN0cmluZyAtPiBTdHJpbmcgLT4gU3RyaW5nXG5yZXBlYXRIZWxwIG4gY2h1bmsgcmVzdWx0ID1cbiAgICBpZiBuIDw9IDAgdGhlblxuICAgICAgICByZXN1bHRcblxuICAgIGVsc2VcbiAgICAgICAgcmVwZWF0SGVscCAoQml0d2lzZS5zaGlmdFJpZ2h0QnkgMSBuKSAoY2h1bmsgKysgY2h1bmspIDx8XG4gICAgICAgICAgICBpZiBCaXR3aXNlLmFuZCBuIDEgPT0gMCB0aGVuXG4gICAgICAgICAgICAgICAgcmVzdWx0XG5cbiAgICAgICAgICAgIGVsc2VcbiAgICAgICAgICAgICAgICByZXN1bHQgKysgY2h1bmtcblxuXG57LXwgUmVwbGFjZSBhbGwgb2NjdXJyZW5jZXMgb2Ygc29tZSBzdWJzdHJpbmcuXG5cbiAgICByZXBsYWNlIFwiLlwiIFwiLVwiIFwiSnNvbi5EZWNvZGUuc3VjY2VlZFwiID09IFwiSnNvbi1EZWNvZGUtc3VjY2VlZFwiXG5cbiAgICByZXBsYWNlIFwiLFwiIFwiL1wiIFwiYSxiLGMsZCxlXCIgPT0gXCJhL2IvYy9kL2VcIlxuXG4qKk5vdGU6KiogSWYgeW91IG5lZWQgbW9yZSBhZHZhbmNlZCByZXBsYWNlbWVudHMsIGNoZWNrIG91dCB0aGVcbltgZ3Jlbi1sYW5nL3BhcnNlcmBdW3BhcnNlcl0gb3IgW2BTdHJpbmcuUmVnZXhgXVtyZWdleF0gcGFja2FnZS5cblxuW3BhcnNlcl06IC9wYWNrYWdlL2dyZW4tbGFuZy9wYXJzZXJcbltyZWdleF06IFN0cmluZy5SZWdleFxuXG4tfVxucmVwbGFjZSA6IFN0cmluZyAtPiBTdHJpbmcgLT4gU3RyaW5nIC0+IFN0cmluZ1xucmVwbGFjZSBiZWZvcmUgYWZ0ZXIgc3RyaW5nID1cbiAgICBqb2luIGFmdGVyIChzcGxpdCBiZWZvcmUgc3RyaW5nKVxuXG5cblxuLS0gQlVJTERJTkcgQU5EIFNQTElUVElOR1xuXG5cbnstfCBDb21iaW5lIHR3byBzdHJpbmdzLiBZb3UgY2FuIGFsc28gdXNlIFt0aGUgYCgrKylgIG9wZXJhdG9yXShCYXNpY3MjKyspXG50byBkbyB0aGlzLlxuXG4gICAgcHJlcGVuZCBcImJ1dHRlclwiIFwiZmx5XCIgPT0gXCJidXR0ZXJmbHlcIlxuXG4tfVxucHJlcGVuZCA6IFN0cmluZyAtPiBTdHJpbmcgLT4gU3RyaW5nXG5wcmVwZW5kID1cbiAgICBHcmVuLktlcm5lbC5TdHJpbmcuYXBwZW5kXG5cblxuey18IEFwcGVuZCBvbmUgc3RyaW5nIG9udG8gYW5vdGhlci4gVGhpcyBpcyB0aGUgc2FtZSBvcGVyYXRpb24gYXMgW3ByZXBlbmRdKHByZXBlbmQpLFxuYnV0IHdpdGggdGhlIGFyZ3VtZW50cyByZXZlcnNlZC5cbiAgICBcbiAgICBhcHBlbmQgXCJidXR0ZXJcIiBcImZseVwiID09IFwiZmx5YnV0dGVyXCJcblxuLX1cbmFwcGVuZCA6IFN0cmluZyAtPiBTdHJpbmcgLT4gU3RyaW5nXG5hcHBlbmQgbGhzIHJocyA9XG4gICAgcHJlcGVuZCByaHMgbGhzXG5cblxuey18IFNwbGl0IGEgc3RyaW5nIHVzaW5nIGEgZ2l2ZW4gc2VwYXJhdG9yLlxuXG4gICAgc3BsaXQgXCIsXCIgXCJjYXQsZG9nLGNvd1wiID09IFsgXCJjYXRcIiwgXCJkb2dcIiwgXCJjb3dcIiBdXG5cbiAgICBzcGxpdCBcIi9cIiBcImhvbWUvZXZhbi9EZXNrdG9wL1wiID09IFsgXCJob21lXCIsIFwiZXZhblwiLCBcIkRlc2t0b3BcIiwgXCJcIiBdXG5cbi19XG5zcGxpdCA6IFN0cmluZyAtPiBTdHJpbmcgLT4gQXJyYXkgU3RyaW5nXG5zcGxpdCA9XG4gICAgR3Jlbi5LZXJuZWwuU3RyaW5nLnNwbGl0XG5cblxuey18IFB1dCBtYW55IHN0cmluZ3MgdG9nZXRoZXIgd2l0aCBhIGdpdmVuIHNlcGFyYXRvci5cblxuICAgIGpvaW4gXCJhXCIgWyBcIkhcIiwgXCJ3XCIsIFwiaWlcIiwgXCJuXCIgXSA9PSBcIkhhd2FpaWFuXCJcblxuICAgIGpvaW4gXCIgXCIgWyBcImNhdFwiLCBcImRvZ1wiLCBcImNvd1wiIF0gPT0gXCJjYXQgZG9nIGNvd1wiXG5cbiAgICBqb2luIFwiL1wiIFsgXCJob21lXCIsIFwiZXZhblwiLCBcIkRlc2t0b3BcIiBdID09IFwiaG9tZS9ldmFuL0Rlc2t0b3BcIlxuXG4tfVxuam9pbiA6IFN0cmluZyAtPiBBcnJheSBTdHJpbmcgLT4gU3RyaW5nXG5qb2luID1cbiAgICBHcmVuLktlcm5lbC5TdHJpbmcuam9pblxuXG5cbnstfCBCcmVhayBhIHN0cmluZyBpbnRvIHdvcmRzLCBzcGxpdHRpbmcgb24gY2h1bmtzIG9mIHdoaXRlc3BhY2UuXG5cbiAgICB3b3JkcyBcIkhvdyBhcmUgXFx0IHlvdT8gXFxuIEdvb2Q/XCIgPT0gWyBcIkhvd1wiLCBcImFyZVwiLCBcInlvdT9cIiwgXCJHb29kP1wiIF1cblxuLX1cbndvcmRzIDogU3RyaW5nIC0+IEFycmF5IFN0cmluZ1xud29yZHMgPVxuICAgIEdyZW4uS2VybmVsLlN0cmluZy53b3Jkc1xuXG5cbnstfCBCcmVhayBhIHN0cmluZyBpbnRvIGxpbmVzLCBzcGxpdHRpbmcgb24gbmV3bGluZXMuXG5cbiAgICBsaW5lcyBcIkhvdyBhcmUgeW91P1xcbkdvb2Q/XCIgPT0gWyBcIkhvdyBhcmUgeW91P1wiLCBcIkdvb2Q/XCIgXVxuXG4tfVxubGluZXMgOiBTdHJpbmcgLT4gQXJyYXkgU3RyaW5nXG5saW5lcyA9XG4gICAgR3Jlbi5LZXJuZWwuU3RyaW5nLmxpbmVzXG5cblxuXG4tLSBTVUJTVFJJTkdTXG5cblxuey18IFRha2UgYSBzdWJzdHJpbmcgZ2l2ZW4gYSBzdGFydCBhbmQgZW5kIGluZGV4LiBOZWdhdGl2ZSBpbmRleGVzXG5hcmUgdGFrZW4gc3RhcnRpbmcgZnJvbSB0aGUgX2VuZF8gb2YgdGhlIGxpc3QuXG5cbiAgICBzbGljZSA3IDkgXCJzbmFrZXMgb24gYSBwbGFuZSFcIiA9PSBcIm9uXCJcblxuICAgIHNsaWNlIDAgNiBcInNuYWtlcyBvbiBhIHBsYW5lIVwiID09IFwic25ha2VzXCJcblxuICAgIHNsaWNlIDAgLTcgXCJzbmFrZXMgb24gYSBwbGFuZSFcIiA9PSBcInNuYWtlcyBvbiBhXCJcblxuICAgIHNsaWNlIC02IC0xIFwic25ha2VzIG9uIGEgcGxhbmUhXCIgPT0gXCJwbGFuZVwiXG5cbi19XG5zbGljZSA6IEludCAtPiBJbnQgLT4gU3RyaW5nIC0+IFN0cmluZ1xuc2xpY2UgPVxuICAgIEdyZW4uS2VybmVsLlN0cmluZy5zbGljZVxuXG5cbnstfCBUYWtlIF9uXyBjaGFyYWN0ZXJzIGZyb20gdGhlIGxlZnQgc2lkZSBvZiBhIHN0cmluZy5cblxuICAgIGxlZnQgMiBcIk11bGRlclwiID09IFwiTXVcIlxuXG4tfVxubGVmdCA6IEludCAtPiBTdHJpbmcgLT4gU3RyaW5nXG5sZWZ0IG4gc3RyaW5nID1cbiAgICBpZiBuIDwgMSB0aGVuXG4gICAgICAgIFwiXCJcblxuICAgIGVsc2VcbiAgICAgICAgc2xpY2UgMCBuIHN0cmluZ1xuXG5cbnstfCBUYWtlIF9uXyBjaGFyYWN0ZXJzIGZyb20gdGhlIHJpZ2h0IHNpZGUgb2YgYSBzdHJpbmcuXG5cbiAgICByaWdodCAyIFwiU2N1bGx5XCIgPT0gXCJseVwiXG5cbi19XG5yaWdodCA6IEludCAtPiBTdHJpbmcgLT4gU3RyaW5nXG5yaWdodCBuIHN0cmluZyA9XG4gICAgaWYgbiA8IDEgdGhlblxuICAgICAgICBcIlwiXG5cbiAgICBlbHNlXG4gICAgICAgIHNsaWNlIC1uIChsZW5ndGggc3RyaW5nKSBzdHJpbmdcblxuXG57LXwgRHJvcCBfbl8gY2hhcmFjdGVycyBmcm9tIHRoZSBsZWZ0IHNpZGUgb2YgYSBzdHJpbmcuXG5cbiAgICBkcm9wTGVmdCAyIFwiVGhlIExvbmUgR3VubWVuXCIgPT0gXCJlIExvbmUgR3VubWVuXCJcblxuLX1cbmRyb3BMZWZ0IDogSW50IC0+IFN0cmluZyAtPiBTdHJpbmdcbmRyb3BMZWZ0IG4gc3RyaW5nID1cbiAgICBpZiBuIDwgMSB0aGVuXG4gICAgICAgIHN0cmluZ1xuXG4gICAgZWxzZVxuICAgICAgICBzbGljZSBuIChsZW5ndGggc3RyaW5nKSBzdHJpbmdcblxuXG57LXwgRHJvcCBfbl8gY2hhcmFjdGVycyBmcm9tIHRoZSByaWdodCBzaWRlIG9mIGEgc3RyaW5nLlxuXG4gICAgZHJvcFJpZ2h0IDIgXCJDaWdhcmV0dGUgU21va2luZyBNYW5cIiA9PSBcIkNpZ2FyZXR0ZSBTbW9raW5nIE1cIlxuXG4tfVxuZHJvcFJpZ2h0IDogSW50IC0+IFN0cmluZyAtPiBTdHJpbmdcbmRyb3BSaWdodCBuIHN0cmluZyA9XG4gICAgaWYgbiA8IDEgdGhlblxuICAgICAgICBzdHJpbmdcblxuICAgIGVsc2VcbiAgICAgICAgc2xpY2UgMCAtbiBzdHJpbmdcblxuXG5cbi0tIERFVEVDVCBTVUJTVFJJTkdTXG5cblxuey18IFNlZSBpZiB0aGUgc2Vjb25kIHN0cmluZyBjb250YWlucyB0aGUgZmlyc3Qgb25lLlxuXG4gICAgY29udGFpbnMgXCJ0aGVcIiBcInRoZW9yeVwiID09IFRydWVcblxuICAgIGNvbnRhaW5zIFwiaGF0XCIgXCJ0aGVvcnlcIiA9PSBGYWxzZVxuXG4gICAgY29udGFpbnMgXCJUSEVcIiBcInRoZW9yeVwiID09IEZhbHNlXG5cbi19XG5jb250YWlucyA6IFN0cmluZyAtPiBTdHJpbmcgLT4gQm9vbFxuY29udGFpbnMgPVxuICAgIEdyZW4uS2VybmVsLlN0cmluZy5jb250YWluc1xuXG5cbnstfCBTZWUgaWYgdGhlIHNlY29uZCBzdHJpbmcgc3RhcnRzIHdpdGggdGhlIGZpcnN0IG9uZS5cblxuICAgIHN0YXJ0c1dpdGggXCJ0aGVcIiBcInRoZW9yeVwiID09IFRydWVcblxuICAgIHN0YXJ0c1dpdGggXCJvcnlcIiBcInRoZW9yeVwiID09IEZhbHNlXG5cbi19XG5zdGFydHNXaXRoIDogU3RyaW5nIC0+IFN0cmluZyAtPiBCb29sXG5zdGFydHNXaXRoID1cbiAgICBHcmVuLktlcm5lbC5TdHJpbmcuc3RhcnRzV2l0aFxuXG5cbnstfCBTZWUgaWYgdGhlIHNlY29uZCBzdHJpbmcgZW5kcyB3aXRoIHRoZSBmaXJzdCBvbmUuXG5cbiAgICBlbmRzV2l0aCBcInRoZVwiIFwidGhlb3J5XCIgPT0gRmFsc2VcblxuICAgIGVuZHNXaXRoIFwib3J5XCIgXCJ0aGVvcnlcIiA9PSBUcnVlXG5cbi19XG5lbmRzV2l0aCA6IFN0cmluZyAtPiBTdHJpbmcgLT4gQm9vbFxuZW5kc1dpdGggPVxuICAgIEdyZW4uS2VybmVsLlN0cmluZy5lbmRzV2l0aFxuXG5cbnstfCBHZXQgYWxsIG9mIHRoZSBpbmRpY2VzIGZvciBhIHN1YnN0cmluZyBpbiBhbm90aGVyIHN0cmluZy5cblxuICAgIGluZGV4ZXMgXCJpXCIgXCJNaXNzaXNzaXBwaVwiID09IFsgMSwgNCwgNywgMTAgXVxuXG4gICAgaW5kZXhlcyBcInNzXCIgXCJNaXNzaXNzaXBwaVwiID09IFsgMiwgNSBdXG5cbiAgICBpbmRleGVzIFwibmVlZGxlXCIgXCJoYXlzdGFja1wiID09IFtdXG5cbi19XG5pbmRpY2VzIDogU3RyaW5nIC0+IFN0cmluZyAtPiBBcnJheSBJbnRcbmluZGljZXMgPVxuICAgIEdyZW4uS2VybmVsLlN0cmluZy5pbmRleGVzXG5cblxuXG4tLSBGT1JNQVRUSU5HXG5cblxuey18IENvbnZlcnQgYSBzdHJpbmcgdG8gYWxsIHVwcGVyIGNhc2UuIFVzZWZ1bCBmb3IgY2FzZS1pbnNlbnNpdGl2ZSBjb21wYXJpc29uc1xuYW5kIFZJUlRVQUwgWUVMTElORy5cblxuICAgIHRvVXBwZXIgXCJza2lubmVyXCIgPT0gXCJTS0lOTkVSXCJcblxuLX1cbnRvVXBwZXIgOiBTdHJpbmcgLT4gU3RyaW5nXG50b1VwcGVyID1cbiAgICBHcmVuLktlcm5lbC5TdHJpbmcudG9VcHBlclxuXG5cbnstfCBDb252ZXJ0IGEgc3RyaW5nIHRvIGFsbCBsb3dlciBjYXNlLiBVc2VmdWwgZm9yIGNhc2UtaW5zZW5zaXRpdmUgY29tcGFyaXNvbnMuXG5cbiAgICB0b0xvd2VyIFwiWC1GSUxFU1wiID09IFwieC1maWxlc1wiXG5cbi19XG50b0xvd2VyIDogU3RyaW5nIC0+IFN0cmluZ1xudG9Mb3dlciA9XG4gICAgR3Jlbi5LZXJuZWwuU3RyaW5nLnRvTG93ZXJcblxuXG57LXwgUGFkIGEgc3RyaW5nIG9uIGJvdGggc2lkZXMgdW50aWwgaXQgaGFzIGEgZ2l2ZW4gbGVuZ3RoLlxuXG4gICAgcGFkIDUgJyAnIFwiMVwiID09IFwiICAxICBcIlxuXG4gICAgcGFkIDUgJyAnIFwiMTFcIiA9PSBcIiAgMTEgXCJcblxuICAgIHBhZCA1ICcgJyBcIjEyMVwiID09IFwiIDEyMSBcIlxuXG4tfVxucGFkIDogSW50IC0+IENoYXIgLT4gU3RyaW5nIC0+IFN0cmluZ1xucGFkIG4gY2hhciBzdHJpbmcgPVxuICAgIGxldFxuICAgICAgICBoYWxmID1cbiAgICAgICAgICAgIEJhc2ljcy50b0Zsb2F0IChuIC0gbGVuZ3RoIHN0cmluZykgLyAyXG4gICAgaW5cbiAgICByZXBlYXQgKGNlaWxpbmcgaGFsZikgKGZyb21DaGFyIGNoYXIpICsrIHN0cmluZyArKyByZXBlYXQgKGZsb29yIGhhbGYpIChmcm9tQ2hhciBjaGFyKVxuXG5cbnstfCBQYWQgYSBzdHJpbmcgb24gdGhlIGxlZnQgdW50aWwgaXQgaGFzIGEgZ2l2ZW4gbGVuZ3RoLlxuXG4gICAgcGFkTGVmdCA1ICcuJyBcIjFcIiA9PSBcIi4uLi4xXCJcblxuICAgIHBhZExlZnQgNSAnLicgXCIxMVwiID09IFwiLi4uMTFcIlxuXG4gICAgcGFkTGVmdCA1ICcuJyBcIjEyMVwiID09IFwiLi4xMjFcIlxuXG4tfVxucGFkTGVmdCA6IEludCAtPiBDaGFyIC0+IFN0cmluZyAtPiBTdHJpbmdcbnBhZExlZnQgbiBjaGFyIHN0cmluZyA9XG4gICAgcmVwZWF0IChuIC0gbGVuZ3RoIHN0cmluZykgKGZyb21DaGFyIGNoYXIpICsrIHN0cmluZ1xuXG5cbnstfCBQYWQgYSBzdHJpbmcgb24gdGhlIHJpZ2h0IHVudGlsIGl0IGhhcyBhIGdpdmVuIGxlbmd0aC5cblxuICAgIHBhZFJpZ2h0IDUgJy4nIFwiMVwiID09IFwiMS4uLi5cIlxuXG4gICAgcGFkUmlnaHQgNSAnLicgXCIxMVwiID09IFwiMTEuLi5cIlxuXG4gICAgcGFkUmlnaHQgNSAnLicgXCIxMjFcIiA9PSBcIjEyMS4uXCJcblxuLX1cbnBhZFJpZ2h0IDogSW50IC0+IENoYXIgLT4gU3RyaW5nIC0+IFN0cmluZ1xucGFkUmlnaHQgbiBjaGFyIHN0cmluZyA9XG4gICAgc3RyaW5nICsrIHJlcGVhdCAobiAtIGxlbmd0aCBzdHJpbmcpIChmcm9tQ2hhciBjaGFyKVxuXG5cbnstfCBHZXQgcmlkIG9mIHdoaXRlc3BhY2Ugb24gYm90aCBzaWRlcyBvZiBhIHN0cmluZy5cblxuICAgIHRyaW0gXCIgIGhhdHMgIFxcblwiID09IFwiaGF0c1wiXG5cbi19XG50cmltIDogU3RyaW5nIC0+IFN0cmluZ1xudHJpbSA9XG4gICAgR3Jlbi5LZXJuZWwuU3RyaW5nLnRyaW1cblxuXG57LXwgR2V0IHJpZCBvZiB3aGl0ZXNwYWNlIG9uIHRoZSBsZWZ0IG9mIGEgc3RyaW5nLlxuXG4gICAgdHJpbUxlZnQgXCIgIGhhdHMgIFxcblwiID09IFwiaGF0cyAgXFxuXCJcblxuLX1cbnRyaW1MZWZ0IDogU3RyaW5nIC0+IFN0cmluZ1xudHJpbUxlZnQgPVxuICAgIEdyZW4uS2VybmVsLlN0cmluZy50cmltTGVmdFxuXG5cbnstfCBHZXQgcmlkIG9mIHdoaXRlc3BhY2Ugb24gdGhlIHJpZ2h0IG9mIGEgc3RyaW5nLlxuXG4gICAgdHJpbVJpZ2h0IFwiICBoYXRzICBcXG5cIiA9PSBcIiAgaGF0c1wiXG5cbi19XG50cmltUmlnaHQgOiBTdHJpbmcgLT4gU3RyaW5nXG50cmltUmlnaHQgPVxuICAgIEdyZW4uS2VybmVsLlN0cmluZy50cmltUmlnaHRcblxuXG5cbi0tIElOVCBDT05WRVJTSU9OU1xuXG5cbnstfCBUcnkgdG8gY29udmVydCBhIHN0cmluZyBpbnRvIGFuIGludCwgZmFpbGluZyBvbiBpbXByb3Blcmx5IGZvcm1hdHRlZCBzdHJpbmdzLlxuXG4gICAgU3RyaW5nLnRvSW50IFwiMTIzXCIgPT0gSnVzdCAxMjNcblxuICAgIFN0cmluZy50b0ludCBcIi00MlwiID09IEp1c3QgLTQyXG5cbiAgICBTdHJpbmcudG9JbnQgXCIzLjFcIiA9PSBOb3RoaW5nXG5cbiAgICBTdHJpbmcudG9JbnQgXCIzMWFcIiA9PSBOb3RoaW5nXG5cbklmIHlvdSBhcmUgZXh0cmFjdGluZyBhIG51bWJlciBmcm9tIHNvbWUgcmF3IHVzZXIgaW5wdXQsIHlvdSB3aWxsIHR5cGljYWxseVxud2FudCB0byB1c2UgW2BNYXliZS53aXRoRGVmYXVsdGBdKE1heWJlI3dpdGhEZWZhdWx0KSB0byBoYW5kbGUgYmFkIGRhdGE6XG5cbiAgICBNYXliZS53aXRoRGVmYXVsdCAwIChTdHJpbmcudG9JbnQgXCI0MlwiKSA9PSA0MlxuXG4gICAgTWF5YmUud2l0aERlZmF1bHQgMCAoU3RyaW5nLnRvSW50IFwiYWJcIikgPT0gMFxuXG4tfVxudG9JbnQgOiBTdHJpbmcgLT4gTWF5YmUgSW50XG50b0ludCA9XG4gICAgR3Jlbi5LZXJuZWwuU3RyaW5nLnRvSW50XG5cblxuey18IENvbnZlcnQgYW4gYEludGAgdG8gYSBgU3RyaW5nYC5cblxuICAgIFN0cmluZy5mcm9tSW50IDEyMyA9PSBcIjEyM1wiXG5cbiAgICBTdHJpbmcuZnJvbUludCAtNDIgPT0gXCItNDJcIlxuXG5DaGVjayBvdXQgW2BEZWJ1Zy50b1N0cmluZ2BdKERlYnVnI3RvU3RyaW5nKSB0byBjb252ZXJ0IF9hbnlfIHZhbHVlIHRvIGEgc3RyaW5nXG5mb3IgZGVidWdnaW5nIHB1cnBvc2VzLlxuXG4tfVxuZnJvbUludCA6IEludCAtPiBTdHJpbmdcbmZyb21JbnQgPVxuICAgIEdyZW4uS2VybmVsLlN0cmluZy5mcm9tTnVtYmVyXG5cblxuXG4tLSBGTE9BVCBDT05WRVJTSU9OU1xuXG5cbnstfCBUcnkgdG8gY29udmVydCBhIHN0cmluZyBpbnRvIGEgZmxvYXQsIGZhaWxpbmcgb24gaW1wcm9wZXJseSBmb3JtYXR0ZWQgc3RyaW5ncy5cblxuICAgIFN0cmluZy50b0Zsb2F0IFwiMTIzXCIgPT0gSnVzdCAxMjMuMFxuXG4gICAgU3RyaW5nLnRvRmxvYXQgXCItNDJcIiA9PSBKdXN0IC00Mi4wXG5cbiAgICBTdHJpbmcudG9GbG9hdCBcIjMuMVwiID09IEp1c3QgMy4xXG5cbiAgICBTdHJpbmcudG9GbG9hdCBcIjMxYVwiID09IE5vdGhpbmdcblxuSWYgeW91IGFyZSBleHRyYWN0aW5nIGEgbnVtYmVyIGZyb20gc29tZSByYXcgdXNlciBpbnB1dCwgeW91IHdpbGwgdHlwaWNhbGx5XG53YW50IHRvIHVzZSBbYE1heWJlLndpdGhEZWZhdWx0YF0oTWF5YmUjd2l0aERlZmF1bHQpIHRvIGhhbmRsZSBiYWQgZGF0YTpcblxuICAgIE1heWJlLndpdGhEZWZhdWx0IDAgKFN0cmluZy50b0Zsb2F0IFwiNDIuNVwiKSA9PSA0Mi41XG5cbiAgICBNYXliZS53aXRoRGVmYXVsdCAwIChTdHJpbmcudG9GbG9hdCBcImNhdHNcIikgPT0gMFxuXG4tfVxudG9GbG9hdCA6IFN0cmluZyAtPiBNYXliZSBGbG9hdFxudG9GbG9hdCA9XG4gICAgR3Jlbi5LZXJuZWwuU3RyaW5nLnRvRmxvYXRcblxuXG57LXwgQ29udmVydCBhIGBGbG9hdGAgdG8gYSBgU3RyaW5nYC5cblxuICAgIFN0cmluZy5mcm9tRmxvYXQgMTIzID09IFwiMTIzXCJcblxuICAgIFN0cmluZy5mcm9tRmxvYXQgLTQyID09IFwiLTQyXCJcblxuICAgIFN0cmluZy5mcm9tRmxvYXQgMy45ID09IFwiMy45XCJcblxuQ2hlY2sgb3V0IFtgRGVidWcudG9TdHJpbmdgXShEZWJ1ZyN0b1N0cmluZykgdG8gY29udmVydCBfYW55XyB2YWx1ZSB0byBhIHN0cmluZ1xuZm9yIGRlYnVnZ2luZyBwdXJwb3Nlcy5cblxuLX1cbmZyb21GbG9hdCA6IEZsb2F0IC0+IFN0cmluZ1xuZnJvbUZsb2F0ID1cbiAgICBHcmVuLktlcm5lbC5TdHJpbmcuZnJvbU51bWJlclxuXG5cblxuLS0gTElTVCBDT05WRVJTSU9OU1xuXG5cbnstfCBDb252ZXJ0IGEgc3RyaW5nIHRvIGEgbGlzdCBvZiBjaGFyYWN0ZXJzLlxuXG4gICAgdG9BcnJheSBcImFiY1wiID09IFsgJ2EnLCAnYicsICdjJyBdXG5cbiAgICB0b0FycmF5IFwi8J+ZiPCfmYnwn5mKXCIgPT0gWyAn8J+ZiCcsICfwn5mJJywgJ/CfmYonIF1cblxuLX1cbnRvQXJyYXkgOiBTdHJpbmcgLT4gQXJyYXkgQ2hhclxudG9BcnJheSBzdHJpbmcgPVxuICAgIGZvbGRsIEFycmF5LnB1c2hMYXN0IFtdIHN0cmluZ1xuXG5cbnstfCBDb252ZXJ0IGEgbGlzdCBvZiBjaGFyYWN0ZXJzIGludG8gYSBTdHJpbmcuIENhbiBiZSB1c2VmdWwgaWYgeW91XG53YW50IHRvIGNyZWF0ZSBhIHN0cmluZyBwcmltYXJpbHkgYnkgY29uc2luZywgcGVyaGFwcyBmb3IgZGVjb2RpbmdcbnNvbWV0aGluZy5cblxuICAgIGZyb21BcnJheSBbICdhJywgJ2InLCAnYycgXSA9PSBcImFiY1wiXG5cbiAgICBmcm9tQXJyYXkgWyAn8J+ZiCcsICfwn5mJJywgJ/CfmYonIF0gPT0gXCLwn5mI8J+ZifCfmYpcIlxuXG4tfVxuZnJvbUFycmF5IDogQXJyYXkgQ2hhciAtPiBTdHJpbmdcbmZyb21BcnJheSA9XG4gICAgR3Jlbi5LZXJuZWwuU3RyaW5nLmZyb21BcnJheVxuXG5cblxuLS0gQ0hBUiBDT05WRVJTSU9OU1xuXG5cbnstfCBDcmVhdGUgYSBzdHJpbmcgZnJvbSBhIGdpdmVuIGNoYXJhY3Rlci5cblxuICAgIGZyb21DaGFyICdhJyA9PSBcImFcIlxuXG4tfVxuZnJvbUNoYXIgOiBDaGFyIC0+IFN0cmluZ1xuZnJvbUNoYXIgY2hhciA9XG4gICAgY29ucyBjaGFyIFwiXCJcblxuXG57LXwgQWRkIGEgY2hhcmFjdGVyIHRvIHRoZSBiZWdpbm5pbmcgb2YgYSBzdHJpbmcuXG5cbiAgICBjb25zICdUJyBcImhlIHRydXRoIGlzIG91dCB0aGVyZVwiID09IFwiVGhlIHRydXRoIGlzIG91dCB0aGVyZVwiXG5cbi19XG5jb25zIDogQ2hhciAtPiBTdHJpbmcgLT4gU3RyaW5nXG5jb25zID1cbiAgICBHcmVuLktlcm5lbC5TdHJpbmcuY29uc1xuXG5cbnstfCBTcGxpdCBhIG5vbi1lbXB0eSBzdHJpbmcgaW50byBpdHMgaGVhZCBhbmQgdGFpbC4gVGhpcyBsZXRzIHlvdVxucGF0dGVybiBtYXRjaCBvbiBzdHJpbmdzIGV4YWN0bHkgYXMgeW91IHdvdWxkIHdpdGggbGlzdHMuXG5cbiAgICB1bmNvbnMgXCJhYmNcIiA9PSBKdXN0ICggJ2EnLCBcImJjXCIgKVxuXG4gICAgdW5jb25zIFwiXCIgPT0gTm90aGluZ1xuXG4tfVxudW5jb25zIDogU3RyaW5nIC0+IE1heWJlIHsgZmlyc3QgOiBDaGFyLCByZXN0IDogU3RyaW5nIH1cbnVuY29ucyA9XG4gICAgR3Jlbi5LZXJuZWwuU3RyaW5nLnVuY29uc1xuXG5cblxuLS0gSElHSEVSLU9SREVSIEZVTkNUSU9OU1xuXG5cbnstfCBUcmFuc2Zvcm0gZXZlcnkgY2hhcmFjdGVyIGluIGEgc3RyaW5nXG5cbiAgICBtYXBcbiAgICAgICAgKFxcYyAtPlxuICAgICAgICAgICAgaWYgYyA9PSAnLycgdGhlblxuICAgICAgICAgICAgICAgICcuJ1xuXG4gICAgICAgICAgICBlbHNlXG4gICAgICAgICAgICAgICAgY1xuICAgICAgICApXG4gICAgICAgIFwiYS9iL2NcIlxuICAgICAgICA9PSBcImEuYi5jXCJcblxuLX1cbm1hcCA6IChDaGFyIC0+IENoYXIpIC0+IFN0cmluZyAtPiBTdHJpbmdcbm1hcCA9XG4gICAgR3Jlbi5LZXJuZWwuU3RyaW5nLm1hcFxuXG5cbnstfCBLZWVwIG9ubHkgdGhlIGNoYXJhY3RlcnMgdGhhdCBwYXNzIHRoZSB0ZXN0LlxuXG4gICAgZmlsdGVyIGlzRGlnaXQgXCJSMi1EMlwiID09IFwiMjJcIlxuXG4tfVxuZmlsdGVyIDogKENoYXIgLT4gQm9vbCkgLT4gU3RyaW5nIC0+IFN0cmluZ1xuZmlsdGVyID1cbiAgICBHcmVuLktlcm5lbC5TdHJpbmcuZmlsdGVyXG5cblxuey18IFJlZHVjZSBhIHN0cmluZyBmcm9tIHRoZSBsZWZ0LlxuXG4gICAgZm9sZGwgY29ucyBcIlwiIFwidGltZVwiID09IFwiZW1pdFwiXG5cbi19XG5mb2xkbCA6IChDaGFyIC0+IGIgLT4gYikgLT4gYiAtPiBTdHJpbmcgLT4gYlxuZm9sZGwgPVxuICAgIEdyZW4uS2VybmVsLlN0cmluZy5mb2xkbFxuXG5cbnstfCBSZWR1Y2UgYSBzdHJpbmcgZnJvbSB0aGUgcmlnaHQuXG5cbiAgICBmb2xkciBjb25zIFwiXCIgXCJ0aW1lXCIgPT0gXCJ0aW1lXCJcblxuLX1cbmZvbGRyIDogKENoYXIgLT4gYiAtPiBiKSAtPiBiIC0+IFN0cmluZyAtPiBiXG5mb2xkciA9XG4gICAgR3Jlbi5LZXJuZWwuU3RyaW5nLmZvbGRyXG5cblxuey18IERldGVybWluZSB3aGV0aGVyIF9hbnlfIGNoYXJhY3RlcnMgcGFzcyB0aGUgdGVzdC5cblxuICAgIGFueSBpc0RpZ2l0IFwiOTAyMTBcIiA9PSBUcnVlXG5cbiAgICBhbnkgaXNEaWdpdCBcIlIyLUQyXCIgPT0gVHJ1ZVxuXG4gICAgYW55IGlzRGlnaXQgXCJoZWFydFwiID09IEZhbHNlXG5cbiAgICBhbnkgaXNEaWdpdCBcIlwiID09IEZhbHNlXG5cbi19XG5hbnkgOiAoQ2hhciAtPiBCb29sKSAtPiBTdHJpbmcgLT4gQm9vbFxuYW55ID1cbiAgICBHcmVuLktlcm5lbC5TdHJpbmcuYW55XG5cblxuey18IERldGVybWluZSB3aGV0aGVyIF9hbGxfIGNoYXJhY3RlcnMgcGFzcyB0aGUgdGVzdC5cblxuICAgIGFsbCBpc0RpZ2l0IFwiOTAyMTBcIiA9PSBUcnVlXG5cbiAgICBhbGwgaXNEaWdpdCBcIlIyLUQyXCIgPT0gRmFsc2VcblxuICAgIGFsbCBpc0RpZ2l0IFwiaGVhcnRcIiA9PSBGYWxzZVxuXG4gICAgYWxsIGlzRGlnaXQgXCJcIiA9IFRydWVcblxuLX1cbmFsbCA6IChDaGFyIC0+IEJvb2wpIC0+IFN0cmluZyAtPiBCb29sXG5hbGwgPVxuICAgIEdyZW4uS2VybmVsLlN0cmluZy5hbGxcbiIsCiAgICAgICAgIm1vZHVsZSBKc29uLkVuY29kZSBleHBvc2luZ1xuICAgICggZW5jb2RlLCBWYWx1ZVxuICAgICwgc3RyaW5nLCBpbnQsIGZsb2F0LCBib29sLCBudWxsXG4gICAgLCBhcnJheSwgc2V0XG4gICAgLCBvYmplY3QsIGRpY3RcbiAgICApXG5cbnstfCBGdW5jdGlvbnMgZm9yIHR1cm5pbmcgR3JlbiB2YWx1ZXMgaW50byBKc29uIHZhbHVlcy5cblxuXG5AZG9jcyBlbmNvZGUsIFZhbHVlXG5cblxuIyMgUHJpbWl0aXZlc1xuXG5AZG9jcyBzdHJpbmcsIGludCwgZmxvYXQsIGJvb2wsIG51bGxcblxuXG4jIyBBcnJheXNcblxuQGRvY3MgYXJyYXksIHNldFxuXG5cbiMjIE9iamVjdHNcblxuQGRvY3Mgb2JqZWN0LCBkaWN0XG5cbi19XG5cbmltcG9ydCBCYXNpY3MgZXhwb3NpbmcgKC4uKVxuaW1wb3J0IEFycmF5IGV4cG9zaW5nIChBcnJheSlcbmltcG9ydCBEaWN0IGV4cG9zaW5nIChEaWN0KVxuaW1wb3J0IFNldCBleHBvc2luZyAoU2V0KVxuaW1wb3J0IFN0cmluZyBleHBvc2luZyAoU3RyaW5nKVxuaW1wb3J0IEdyZW4uS2VybmVsLkpzb25cblxuXG5cbi0tIEVOQ09ERVxuXG5cbnstfCBSZXByZXNlbnRzIGEgSmF2YVNjcmlwdCB2YWx1ZS5cbi19XG50eXBlIFZhbHVlXG4gICAgPSBWYWx1ZVxuXG5cbnstfCBDb252ZXJ0IGEgYFZhbHVlYCBpbnRvIGEgcHJldHRpZmllZCBzdHJpbmcuIFRoZSBmaXJzdCBhcmd1bWVudCBzcGVjaWZpZXNcbnRoZSBhbW91bnQgb2YgaW5kZW50YXRpb24gaW4gdGhlIHJlc3VsdGluZyBzdHJpbmcuXG5cbiAgICBpbXBvcnQgSnNvbi5FbmNvZGUgYXMgRW5jb2RlXG5cbiAgICB0b20gOiBFbmNvZGUuVmFsdWVcbiAgICB0b20gPVxuICAgICAgICBFbmNvZGUub2JqZWN0XG4gICAgICAgICAgICBbIHsga2V5ID0gXCJuYW1lXCIsIHZhbHVlID0gRW5jb2RlLnN0cmluZyBcIlRvbVwiIH1cbiAgICAgICAgICAgICwgeyBrZXkgPSBcImFnZVwiLCB2YWx1ZSA9IEVuY29kZS5pbnQgNDIgKVxuICAgICAgICAgICAgXVxuXG4gICAgY29tcGFjdCA9XG4gICAgICAgIEVuY29kZS5lbmNvZGUgMCB0b21cblxuICAgIC0tIHtcIm5hbWVcIjpcIlRvbVwiLFwiYWdlXCI6NDJ9XG4gICAgcmVhZGFibGUgPVxuICAgICAgICBFbmNvZGUuZW5jb2RlIDQgdG9tXG5cbiAgICAtLSB7XG4gICAgLS0gICAgIFwibmFtZVwiOiBcIlRvbVwiLFxuICAgIC0tICAgICBcImFnZVwiOiA0MlxuICAgIC0tIH1cblxuLX1cbmVuY29kZSA6IEludCAtPiBWYWx1ZSAtPiBTdHJpbmdcbmVuY29kZSA9XG4gICAgR3Jlbi5LZXJuZWwuSnNvbi5lbmNvZGVcblxuXG5cbi0tIFBSSU1JVElWRVNcblxuXG57LXwgVHVybiBhIGBTdHJpbmdgIGludG8gYSBKU09OIHN0cmluZy5cblxuICAgIGltcG9ydCBKc29uLkVuY29kZSBleHBvc2luZyAoZW5jb2RlLCBzdHJpbmcpXG5cblxuICAgIC0tIGVuY29kZSAwIChzdHJpbmcgXCJcIikgICAgICA9PSBcIlxcXCJcXFwiXCJcbiAgICAtLSBlbmNvZGUgMCAoc3RyaW5nIFwiYWJjXCIpICAgPT0gXCJcXFwiYWJjXFxcIlwiXG4gICAgLS0gZW5jb2RlIDAgKHN0cmluZyBcImhlbGxvXCIpID09IFwiXFxcImhlbGxvXFxcIlwiXG5cbi19XG5zdHJpbmcgOiBTdHJpbmcgLT4gVmFsdWVcbnN0cmluZyA9XG4gICAgR3Jlbi5LZXJuZWwuSnNvbi53cmFwXG5cblxuey18IFR1cm4gYW4gYEludGAgaW50byBhIEpTT04gbnVtYmVyLlxuXG4gICAgaW1wb3J0IEpzb24uRW5jb2RlIGV4cG9zaW5nIChlbmNvZGUsIGludClcblxuXG4gICAgLS0gZW5jb2RlIDAgKGludCA0MikgPT0gXCI0MlwiXG4gICAgLS0gZW5jb2RlIDAgKGludCAtNykgPT0gXCItN1wiXG4gICAgLS0gZW5jb2RlIDAgKGludCAwKSAgPT0gXCIwXCJcblxuLX1cbmludCA6IEludCAtPiBWYWx1ZVxuaW50ID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLndyYXBcblxuXG57LXwgVHVybiBhIGBGbG9hdGAgaW50byBhIEpTT04gbnVtYmVyLlxuXG4gICAgaW1wb3J0IEpzb24uRW5jb2RlIGV4cG9zaW5nIChlbmNvZGUsIGZsb2F0KVxuXG5cbiAgICAtLSBlbmNvZGUgMCAoZmxvYXQgMy4xNCkgICAgID09IFwiMy4xNFwiXG4gICAgLS0gZW5jb2RlIDAgKGZsb2F0IDEuNjE4KSAgICA9PSBcIjEuNjE4XCJcbiAgICAtLSBlbmNvZGUgMCAoZmxvYXQgLTQyKSAgICAgID09IFwiLTQyXCJcbiAgICAtLSBlbmNvZGUgMCAoZmxvYXQgTmFOKSAgICAgID09IFwibnVsbFwiXG4gICAgLS0gZW5jb2RlIDAgKGZsb2F0IEluZmluaXR5KSA9PSBcIm51bGxcIlxuXG4qKk5vdGU6KiogRmxvYXRpbmcgcG9pbnQgbnVtYmVycyBhcmUgZGVmaW5lZCBpbiB0aGUgW0lFRUUgNzU0IHN0YW5kYXJkXVtpZWVlXVxud2hpY2ggaXMgaGFyZGNvZGVkIGludG8gYWxtb3N0IGFsbCBDUFVzLiBUaGlzIHN0YW5kYXJkIGFsbG93cyBgSW5maW5pdHlgIGFuZFxuYE5hTmAuIFtUaGUgSlNPTiBzcGVjXVtqc29uXSBkb2VzIG5vdCBpbmNsdWRlIHRoZXNlIHZhbHVlcywgc28gd2UgZW5jb2RlIHRoZW1cbmJvdGggYXMgYG51bGxgLlxuXG5baWVlZV06IGh0dHBzOi8vZW4ud2lraXBlZGlhLm9yZy93aWtpL0lFRUVfNzU0XG5banNvbl06IGh0dHBzOi8vd3d3Lmpzb24ub3JnL1xuXG4tfVxuZmxvYXQgOiBGbG9hdCAtPiBWYWx1ZVxuZmxvYXQgPVxuICAgIEdyZW4uS2VybmVsLkpzb24ud3JhcFxuXG5cbnstfCBUdXJuIGEgYEJvb2xgIGludG8gYSBKU09OIGJvb2xlYW4uXG5cbiAgICBpbXBvcnQgSnNvbi5FbmNvZGUgZXhwb3NpbmcgKGJvb2wsIGVuY29kZSlcblxuXG4gICAgLS0gZW5jb2RlIDAgKGJvb2wgVHJ1ZSkgID09IFwidHJ1ZVwiXG4gICAgLS0gZW5jb2RlIDAgKGJvb2wgRmFsc2UpID09IFwiZmFsc2VcIlxuXG4tfVxuYm9vbCA6IEJvb2wgLT4gVmFsdWVcbmJvb2wgPVxuICAgIEdyZW4uS2VybmVsLkpzb24ud3JhcFxuXG5cblxuLS0gTlVMTFNcblxuXG57LXwgQ3JlYXRlIGEgSlNPTiBgbnVsbGAgdmFsdWUuXG5cbiAgICBpbXBvcnQgSnNvbi5FbmNvZGUgZXhwb3NpbmcgKGVuY29kZSwgbnVsbClcblxuXG4gICAgLS0gZW5jb2RlIDAgbnVsbCA9PSBcIm51bGxcIlxuXG4tfVxubnVsbCA6IFZhbHVlXG5udWxsID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLmVuY29kZU51bGxcblxuXG5cbi0tIEFSUkFZU1xuXG5cbnstfCBUdXJuIGEgYEFycmF5YCBpbnRvIGEgSlNPTiBhcnJheS5cblxuICAgIGltcG9ydCBKc29uLkVuY29kZSBhcyBFbmNvZGUgZXhwb3NpbmcgKGFycmF5LCBib29sLCBlbmNvZGUsIGludCwgc3RyaW5nKVxuXG5cbiAgICAtLSBlbmNvZGUgMCAoYXJyYXkgaW50IFsxLDMsNF0pICAgICAgID09IFwiWzEsMyw0XVwiXG4gICAgLS0gZW5jb2RlIDAgKGFycmF5IGJvb2wgW1RydWUsRmFsc2VdKSA9PSBcIlt0cnVlLGZhbHNlXVwiXG4gICAgLS0gZW5jb2RlIDAgKGFycmF5IHN0cmluZyBbXCJhXCIsXCJiXCJdKSAgPT0gXCJcIlwiW1wiYVwiLFwiYlwiXVwiXCJcIlxuXG4tfVxuYXJyYXkgOiAoYSAtPiBWYWx1ZSkgLT4gQXJyYXkgYSAtPiBWYWx1ZVxuYXJyYXkgZnVuYyBlbnRyaWVzID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLndyYXBcbiAgICAgICAgKEFycmF5LmZvbGRsIChHcmVuLktlcm5lbC5Kc29uLmFkZEVudHJ5IGZ1bmMpIChHcmVuLktlcm5lbC5Kc29uLmVtcHR5QXJyYXkge30pIGVudHJpZXMpXG5cblxuey18IFR1cm4gYW4gYFNldGAgaW50byBhIEpTT04gYXJyYXkuXG4tfVxuc2V0IDogKGEgLT4gVmFsdWUpIC0+IFNldCBhIC0+IFZhbHVlXG5zZXQgZnVuYyBlbnRyaWVzID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLndyYXBcbiAgICAgICAgKFNldC5mb2xkbCAoR3Jlbi5LZXJuZWwuSnNvbi5hZGRFbnRyeSBmdW5jKSAoR3Jlbi5LZXJuZWwuSnNvbi5lbXB0eUFycmF5IHt9KSBlbnRyaWVzKVxuXG5cblxuLS0gT0JKRUNUU1xuXG5cbnstfCBDcmVhdGUgYSBKU09OIG9iamVjdC5cblxuICAgIGltcG9ydCBKc29uLkVuY29kZSBhcyBFbmNvZGVcblxuICAgIHRvbSA6IEVuY29kZS5WYWx1ZVxuICAgIHRvbSA9XG4gICAgICAgIEVuY29kZS5vYmplY3RcbiAgICAgICAgICAgIFsgeyBrZXkgPSBcIm5hbWVcIiwgdmFsdWUgPSBFbmNvZGUuc3RyaW5nIFwiVG9tXCIgfVxuICAgICAgICAgICAgLCB7IGtleSA9IFwiYWdlXCIsIHZhbHVlID0gRW5jb2RlLmludCA0MiB9XG4gICAgICAgICAgICBdXG5cbiAgICAtLSBFbmNvZGUuZW5jb2RlIDAgdG9tID09IFwiXCJcIntcIm5hbWVcIjpcIlRvbVwiLFwiYWdlXCI6NDJ9XCJcIlwiXG5cbi19XG5vYmplY3QgOiBBcnJheSB7IGtleSA6IFN0cmluZywgdmFsdWUgOiBWYWx1ZSB9IC0+IFZhbHVlXG5vYmplY3QgcGFpcnMgPVxuICAgIEdyZW4uS2VybmVsLkpzb24ud3JhcFxuICAgICAgICAoQXJyYXkuZm9sZGxcbiAgICAgICAgICAgIChcXHsga2V5LCB2YWx1ZSB9IG9iaiAtPiBHcmVuLktlcm5lbC5Kc29uLmFkZEZpZWxkIGtleSB2YWx1ZSBvYmopXG4gICAgICAgICAgICAoR3Jlbi5LZXJuZWwuSnNvbi5lbXB0eU9iamVjdCB7fSlcbiAgICAgICAgICAgIHBhaXJzXG4gICAgICAgIClcblxuXG57LXwgVHVybiBhIGBEaWN0YCBpbnRvIGEgSlNPTiBvYmplY3QuXG5cbiAgICBpbXBvcnQgRGljdCBleHBvc2luZyAoRGljdClcbiAgICBpbXBvcnQgSnNvbi5FbmNvZGUgYXMgRW5jb2RlXG5cbiAgICBwZW9wbGUgOiBEaWN0IFN0cmluZyBJbnRcbiAgICBwZW9wbGUgPVxuICAgICAgICBEaWN0LmZyb21BcnJheSBbIHsga2V5ID0gXCJUb21cIiwgdmFsdWUgPSA0MiB9LCB7IGtleSA9IFwiU3VlXCIsIHZhbHVlID0gMzggfSBdXG5cbiAgICAtLSBFbmNvZGUuZW5jb2RlIDAgKEVuY29kZS5kaWN0IGlkZW50aXR5IEVuY29kZS5pbnQgcGVvcGxlKVxuICAgIC0tICAgPT0gXCJcIlwie1wiVG9tXCI6NDIsXCJTdWVcIjozOH1cIlwiXCJcblxuLX1cbmRpY3QgOiAoayAtPiBTdHJpbmcpIC0+ICh2IC0+IFZhbHVlKSAtPiBEaWN0IGsgdiAtPiBWYWx1ZVxuZGljdCB0b0tleSB0b1ZhbHVlIGRpY3Rpb25hcnkgPVxuICAgIEdyZW4uS2VybmVsLkpzb24ud3JhcFxuICAgICAgICAoRGljdC5mb2xkbFxuICAgICAgICAgICAgKFxca2V5IHZhbHVlIG9iaiAtPiBHcmVuLktlcm5lbC5Kc29uLmFkZEZpZWxkICh0b0tleSBrZXkpICh0b1ZhbHVlIHZhbHVlKSBvYmopXG4gICAgICAgICAgICAoR3Jlbi5LZXJuZWwuSnNvbi5lbXB0eU9iamVjdCB7fSlcbiAgICAgICAgICAgIGRpY3Rpb25hcnlcbiAgICAgICAgKVxuIiwKICAgICAgICAibW9kdWxlIEpzb24uRGVjb2RlIGV4cG9zaW5nXG4gICAgKCBEZWNvZGVyLCBzdHJpbmcsIGJvb2wsIGludCwgZmxvYXRcbiAgICAsIG51bGxhYmxlLCBhcnJheSwgZGljdCwga2V5VmFsdWVQYWlycywgb25lT3JNb3JlXG4gICAgLCBmaWVsZCwgYXQsIGluZGV4XG4gICAgLCBtYXliZSwgb25lT2ZcbiAgICAsIGRlY29kZVN0cmluZywgZGVjb2RlVmFsdWUsIFZhbHVlLCBFcnJvciguLiksIGVycm9yVG9TdHJpbmdcbiAgICAsIG1hcCwgbWFwMiwgbWFwMywgbWFwNCwgbWFwNSwgbWFwNiwgbWFwNywgbWFwOFxuICAgICwgbGF6eSwgdmFsdWUsIG51bGwsIHN1Y2NlZWQsIGZhaWwsIGFuZFRoZW5cbiAgICApXG5cbnstfCBUdXJuIEpTT04gdmFsdWVzIGludG8gR3JlbiB2YWx1ZXMuIFdlJ3ZlIGluaGVyaXRlZCB0aGlzIGZyb20gRWxtLiBEZWZpbml0ZWx5IGNoZWNrIG91dCB0aGlzIFtpbnRybyB0b1xuSlNPTiBkZWNvZGVyc11bZ3VpZGVdIHRvIGdldCBhIGZlZWwgZm9yIGhvdyB0aGlzIGxpYnJhcnkgd29ya3MhXG5cbltndWlkZV06IGh0dHBzOi8vZ3VpZGUuZWxtLWxhbmcub3JnL2VmZmVjdHMvanNvbi5odG1sXG5cblxuQGRvY3MgRGVjb2Rlciwgc3RyaW5nLCBib29sLCBpbnQsIGZsb2F0XG5cblxuIyMgRGF0YSBTdHJ1Y3R1cmVzXG5cbkBkb2NzIG51bGxhYmxlLCBhcnJheSwgZGljdCwga2V5VmFsdWVQYWlycywgb25lT3JNb3JlXG5cblxuIyMgT2JqZWN0IFByaW1pdGl2ZXNcblxuQGRvY3MgZmllbGQsIGF0LCBpbmRleFxuXG5cbiMjIEluY29uc2lzdGVudCBTdHJ1Y3R1cmVcblxuQGRvY3MgbWF5YmUsIG9uZU9mXG5cblxuIyMgUnVuIERlY29kZXJzXG5cbkBkb2NzIGRlY29kZVN0cmluZywgZGVjb2RlVmFsdWUsIFZhbHVlLCBFcnJvciwgZXJyb3JUb1N0cmluZ1xuXG5cbiMjIE1hcHBpbmdcblxuQGRvY3MgbWFwLCBtYXAyLCBtYXAzLCBtYXA0LCBtYXA1LCBtYXA2LCBtYXA3LCBtYXA4XG5cblxuIyMgRmFuY3kgRGVjb2RpbmdcblxuQGRvY3MgbGF6eSwgdmFsdWUsIG51bGwsIHN1Y2NlZWQsIGZhaWwsIGFuZFRoZW5cblxuLX1cblxuaW1wb3J0IEJhc2ljcyBleHBvc2luZyAoLi4pXG5pbXBvcnQgQXJyYXkgZXhwb3NpbmcgKEFycmF5KVxuaW1wb3J0IERpY3QgZXhwb3NpbmcgKERpY3QpXG5pbXBvcnQgQ2hhclxuaW1wb3J0IFN0cmluZyBleHBvc2luZyAoU3RyaW5nKVxuaW1wb3J0IE1heWJlIGV4cG9zaW5nIChNYXliZSguLikpXG5pbXBvcnQgUmVzdWx0IGV4cG9zaW5nIChSZXN1bHQoLi4pKVxuaW1wb3J0IEdyZW4uS2VybmVsLkpzb25cbmltcG9ydCBKc29uLkVuY29kZVxuXG5cblxuLS0gUFJJTUlUSVZFU1xuXG5cbnstfCBBIHZhbHVlIHRoYXQga25vd3MgaG93IHRvIGRlY29kZSBKU09OIHZhbHVlcy5cblxuVGhlcmUgaXMgYSB3aG9sZSBzZWN0aW9uIGluIGBndWlkZS5lbG0tbGFuZy5vcmdgIGFib3V0IGRlY29kZXJzLCBzbyBbY2hlY2sgaXRcbm91dF0oaHR0cHM6Ly9ndWlkZS5lbG0tbGFuZy5vcmcvaW50ZXJvcC9qc29uLmh0bWwpIGZvciBhIG1vcmUgY29tcHJlaGVuc2l2ZVxuaW50cm9kdWN0aW9uIVxuXG4tfVxudHlwZSBEZWNvZGVyIGFcbiAgICA9IERlY29kZXJcblxuXG57LXwgRGVjb2RlIGEgSlNPTiBzdHJpbmcgaW50byBhbiBHcmVuIGBTdHJpbmdgLlxuXG4gICAgZGVjb2RlU3RyaW5nIHN0cmluZyBcInRydWVcIiAgICAgICAgICAgICAgPT0gRXJyIC4uLlxuICAgIGRlY29kZVN0cmluZyBzdHJpbmcgXCI0MlwiICAgICAgICAgICAgICAgID09IEVyciAuLi5cbiAgICBkZWNvZGVTdHJpbmcgc3RyaW5nIFwiMy4xNFwiICAgICAgICAgICAgICA9PSBFcnIgLi4uXG4gICAgZGVjb2RlU3RyaW5nIHN0cmluZyBcIlxcXCJoZWxsb1xcXCJcIiAgICAgICAgID09IE9rIFwiaGVsbG9cIlxuICAgIGRlY29kZVN0cmluZyBzdHJpbmcgXCJ7IFxcXCJoZWxsb1xcXCI6IDQyIH1cIiA9PSBFcnIgLi4uXG5cbi19XG5zdHJpbmcgOiBEZWNvZGVyIFN0cmluZ1xuc3RyaW5nID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLmRlY29kZVN0cmluZ1xuXG5cbnstfCBEZWNvZGUgYSBKU09OIGJvb2xlYW4gaW50byBhbiBHcmVuIGBCb29sYC5cblxuICAgIGRlY29kZVN0cmluZyBib29sIFwidHJ1ZVwiICAgICAgICAgICAgICA9PSBPayBUcnVlXG4gICAgZGVjb2RlU3RyaW5nIGJvb2wgXCI0MlwiICAgICAgICAgICAgICAgID09IEVyciAuLi5cbiAgICBkZWNvZGVTdHJpbmcgYm9vbCBcIjMuMTRcIiAgICAgICAgICAgICAgPT0gRXJyIC4uLlxuICAgIGRlY29kZVN0cmluZyBib29sIFwiXFxcImhlbGxvXFxcIlwiICAgICAgICAgPT0gRXJyIC4uLlxuICAgIGRlY29kZVN0cmluZyBib29sIFwieyBcXFwiaGVsbG9cXFwiOiA0MiB9XCIgPT0gRXJyIC4uLlxuXG4tfVxuYm9vbCA6IERlY29kZXIgQm9vbFxuYm9vbCA9XG4gICAgR3Jlbi5LZXJuZWwuSnNvbi5kZWNvZGVCb29sXG5cblxuey18IERlY29kZSBhIEpTT04gbnVtYmVyIGludG8gYW4gR3JlbiBgSW50YC5cblxuICAgIGRlY29kZVN0cmluZyBpbnQgXCJ0cnVlXCIgICAgICAgICAgICAgID09IEVyciAuLi5cbiAgICBkZWNvZGVTdHJpbmcgaW50IFwiNDJcIiAgICAgICAgICAgICAgICA9PSBPayA0MlxuICAgIGRlY29kZVN0cmluZyBpbnQgXCIzLjE0XCIgICAgICAgICAgICAgID09IEVyciAuLi5cbiAgICBkZWNvZGVTdHJpbmcgaW50IFwiXFxcImhlbGxvXFxcIlwiICAgICAgICAgPT0gRXJyIC4uLlxuICAgIGRlY29kZVN0cmluZyBpbnQgXCJ7IFxcXCJoZWxsb1xcXCI6IDQyIH1cIiA9PSBFcnIgLi4uXG5cbi19XG5pbnQgOiBEZWNvZGVyIEludFxuaW50ID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLmRlY29kZUludFxuXG5cbnstfCBEZWNvZGUgYSBKU09OIG51bWJlciBpbnRvIGFuIEdyZW4gYEZsb2F0YC5cblxuICAgIGRlY29kZVN0cmluZyBmbG9hdCBcInRydWVcIiAgICAgICAgICAgICAgPT0gRXJyIC4uXG4gICAgZGVjb2RlU3RyaW5nIGZsb2F0IFwiNDJcIiAgICAgICAgICAgICAgICA9PSBPayA0MlxuICAgIGRlY29kZVN0cmluZyBmbG9hdCBcIjMuMTRcIiAgICAgICAgICAgICAgPT0gT2sgMy4xNFxuICAgIGRlY29kZVN0cmluZyBmbG9hdCBcIlxcXCJoZWxsb1xcXCJcIiAgICAgICAgID09IEVyciAuLi5cbiAgICBkZWNvZGVTdHJpbmcgZmxvYXQgXCJ7IFxcXCJoZWxsb1xcXCI6IDQyIH1cIiA9PSBFcnIgLi4uXG5cbi19XG5mbG9hdCA6IERlY29kZXIgRmxvYXRcbmZsb2F0ID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLmRlY29kZUZsb2F0XG5cblxuXG4tLSBEQVRBIFNUUlVDVFVSRVNcblxuXG57LXwgRGVjb2RlIGEgbnVsbGFibGUgSlNPTiB2YWx1ZSBpbnRvIGFuIEdyZW4gdmFsdWUuXG5cbiAgICBkZWNvZGVTdHJpbmcgKG51bGxhYmxlIGludCkgXCIxM1wiICAgID09IE9rIChKdXN0IDEzKVxuICAgIGRlY29kZVN0cmluZyAobnVsbGFibGUgaW50KSBcIjQyXCIgICAgPT0gT2sgKEp1c3QgNDIpXG4gICAgZGVjb2RlU3RyaW5nIChudWxsYWJsZSBpbnQpIFwibnVsbFwiICA9PSBPayBOb3RoaW5nXG4gICAgZGVjb2RlU3RyaW5nIChudWxsYWJsZSBpbnQpIFwidHJ1ZVwiICA9PSBFcnIgLi5cblxuLX1cbm51bGxhYmxlIDogRGVjb2RlciBhIC0+IERlY29kZXIgKE1heWJlIGEpXG5udWxsYWJsZSBkZWNvZGVyID1cbiAgICBvbmVPZlxuICAgICAgICBbIG51bGwgTm90aGluZ1xuICAgICAgICAsIG1hcCBKdXN0IGRlY29kZXJcbiAgICAgICAgXVxuXG5cbnstfCBEZWNvZGUgYSBKU09OIGFycmF5IGludG8gYW4gR3JlbiBgQXJyYXlgLlxuXG4gICAgZGVjb2RlU3RyaW5nIChhcnJheSBpbnQpIFwiWzEsMiwzXVwiID09IE9rIFsgMSwgMiwgMyBdXG5cbiAgICBkZWNvZGVTdHJpbmcgKGFycmF5IGJvb2wpIFwiW3RydWUsZmFsc2VdXCIgPT0gT2sgWyBUcnVlLCBGYWxzZSBdXG5cbi19XG5hcnJheSA6IERlY29kZXIgYSAtPiBEZWNvZGVyIChBcnJheSBhKVxuYXJyYXkgPVxuICAgIEdyZW4uS2VybmVsLkpzb24uZGVjb2RlQXJyYXlcblxuXG57LXwgRGVjb2RlIGEgSlNPTiBvYmplY3QgaW50byBhbiBHcmVuIGBEaWN0YC5cblxuICAgIGRlY29kZVN0cmluZyAoZGljdCBpbnQpIFwieyBcXFwiYWxpY2VcXFwiOiA0MiwgXFxcImJvYlxcXCI6IDk5IH1cIlxuICAgICAgICA9PSBPayAoRGljdC5lbXB0eSB8PiBEaWN0LnNldCBcImFsaWNlXCIgNDIgfD4gRGljdC5zZXQgXCJib2JcIiA5OSlcblxuSWYgeW91IG5lZWQgdGhlIGtleXMgKGxpa2UgYFwiYWxpY2VcImAgYW5kIGBcImJvYlwiYCkgYXZhaWxhYmxlIGluIHRoZSBgRGljdGBcbnZhbHVlcyBhcyB3ZWxsLCBJIHJlY29tbWVuZCB1c2luZyBhIChwcml2YXRlKSBpbnRlcm1lZGlhdGUgZGF0YSBzdHJ1Y3R1cmUgbGlrZVxuYEluZm9gIGluIHRoaXMgZXhhbXBsZTpcblxuICAgIG1vZHVsZSBVc2VyIGV4cG9zaW5nICggVXNlciwgZGVjb2RlciApXG5cbiAgICBpbXBvcnQgRGljdFxuICAgIGltcG9ydCBKc29uLkRlY29kZSBleHBvc2luZyAoLi4pXG5cbiAgICB0eXBlIGFsaWFzIFVzZXIgPVxuICAgICAgICB7IG5hbWUgOiBTdHJpbmdcbiAgICAgICAgLCBoZWlnaHQgOiBGbG9hdFxuICAgICAgICAsIGFnZSA6IEludFxuICAgICAgICB9XG5cbiAgICBtYWtlVXNlciA6IFN0cmluZyAtPiBGbG9hdCAtPiBJbnQgLT4gVXNlclxuICAgIG1ha2VVc2VyIG5hbWUgaGVpZ2h0IGFnZSA9XG4gICAgICAgIHsgbmFtZSA9IG5hbWVcbiAgICAgICAgLCBoZWlnaHQgPSBoZWlnaHRcbiAgICAgICAgLCBhZ2UgPSBhZ2VcbiAgICAgICAgfVxuXG4gICAgZGVjb2RlciA6IERlY29kZXIgKERpY3QuRGljdCBTdHJpbmcgVXNlcilcbiAgICBkZWNvZGVyID1cbiAgICAgICAgbWFwIChEaWN0Lm1hcCBpbmZvVG9Vc2VyKSAoZGljdCBpbmZvRGVjb2RlcilcblxuICAgIHR5cGUgYWxpYXMgSW5mbyA9XG4gICAgICAgIHsgaGVpZ2h0IDogRmxvYXRcbiAgICAgICAgLCBhZ2UgOiBJbnRcbiAgICAgICAgfVxuXG4gICAgbWFrZUluZm8gOiBGbG9hdCAtPiBJbnQgLT4gSW5mb1xuICAgIG1ha2VJbmZvIGhlaWdodCBhZ2UgPVxuICAgICAgICB7IGhlaWdodCA9IGhlaWdodFxuICAgICAgICAsIGFnZSA9IGFnZVxuICAgICAgICB9XG5cbiAgICBpbmZvRGVjb2RlciA6IERlY29kZXIgSW5mb1xuICAgIGluZm9EZWNvZGVyID1cbiAgICAgICAgbWFwMiBtYWtlSW5mb1xuICAgICAgICAgICAgKGZpZWxkIFwiaGVpZ2h0XCIgZmxvYXQpXG4gICAgICAgICAgICAoZmllbGQgXCJhZ2VcIiBpbnQpXG5cbiAgICBpbmZvVG9Vc2VyIDogU3RyaW5nIC0+IEluZm8gLT4gVXNlclxuICAgIGluZm9Ub1VzZXIgbmFtZSB7IGhlaWdodCwgYWdlIH0gPVxuICAgICAgICBtYWtlVXNlciBuYW1lIGhlaWdodCBhZ2VcblxuU28gbm93IEpTT04gbGlrZSBgeyBcImFsaWNlXCI6IHsgaGVpZ2h0OiAxLjYsIGFnZTogMzMgfX1gIGFyZSB0dXJuZWQgaW50b1xuZGljdGlvbmFyeSB2YWx1ZXMgbGlrZSBgRGljdC5zaW5nbGV0b24gXCJhbGljZVwiIChVc2VyIFwiYWxpY2VcIiAxLjYgMzMpYCBpZlxueW91IG5lZWQgdGhhdC5cblxuLX1cbmRpY3QgOiBEZWNvZGVyIGEgLT4gRGVjb2RlciAoRGljdCBTdHJpbmcgYSlcbmRpY3QgZGVjb2RlciA9XG4gICAgbWFwIChcXHBhaXJzIC0+IEFycmF5LmZvbGRsIChcXHAgY29sbCAtPiBEaWN0LnNldCBwLmtleSBwLnZhbHVlIGNvbGwpIERpY3QuZW1wdHkgcGFpcnMpIChrZXlWYWx1ZVBhaXJzIGRlY29kZXIpXG5cblxuey18IERlY29kZSBhIEpTT04gb2JqZWN0IGludG8gYW4gR3JlbiBgQXJyYXlgIG9mIHBhaXJzLlxuXG4gICAgZGVjb2RlU3RyaW5nIChrZXlWYWx1ZVBhaXJzIGludCkgXCJ7IFxcXCJhbGljZVxcXCI6IDQyLCBcXFwiYm9iXFxcIjogOTkgfVwiXG4gICAgICAgID09IE9rIFsgeyBrZXkgPSBcImFsaWNlXCIsIHZhbHVlID0gNDIgfSwgeyBrZXkgPSBcImJvYlwiLCB2YWx1ZSA9IDk5IH0gXVxuXG4tfVxua2V5VmFsdWVQYWlycyA6IERlY29kZXIgYSAtPiBEZWNvZGVyIChBcnJheSB7IGtleSA6IFN0cmluZywgdmFsdWUgOiBhIH0pXG5rZXlWYWx1ZVBhaXJzID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLmRlY29kZUtleVZhbHVlUGFpcnNcblxuXG57LXwgRGVjb2RlIGEgSlNPTiBhcnJheSB0aGF0IGhhcyBvbmUgb3IgbW9yZSBlbGVtZW50cy4gVGhpcyBjb21lcyB1cCBpZiB5b3VcbndhbnQgdG8gZW5hYmxlIGRyYWctYW5kLWRyb3Agb2YgZmlsZXMgaW50byB5b3VyIGFwcGxpY2F0aW9uLiBZb3Ugd291bGQgcGFpclxudGhpcyBmdW5jdGlvbiB3aXRoIFtgZWxtL2ZpbGVgXSgpIHRvIHdyaXRlIGEgYGRyb3BEZWNvZGVyYCBsaWtlIHRoaXM6XG5cbiAgICBpbXBvcnQgRmlsZSBleHBvc2luZyAoRmlsZSlcbiAgICBpbXBvcnQgSnNvbi5EZWNvZGVyIGFzIERcblxuICAgIHR5cGUgTXNnXG4gICAgICAgID0gR290RmlsZXMgRmlsZSAoQXJyYXkgRmlsZXMpXG5cbiAgICBpbnB1dERlY29kZXIgOiBELkRlY29kZXIgTXNnXG4gICAgaW5wdXREZWNvZGVyID1cbiAgICAgICAgRC5hdCBbIFwiZGF0YVRyYW5zZmVyXCIsIFwiZmlsZXNcIiBdIChELm9uZU9yTW9yZSBHb3RGaWxlcyBGaWxlLmRlY29kZXIpXG5cblRoaXMgY2FwdHVyZXMgdGhlIGZhY3QgdGhhdCB5b3UgY2FuIG5ldmVyIGRyYWctYW5kLWRyb3AgemVybyBmaWxlcy5cblxuLX1cbm9uZU9yTW9yZSA6IChhIC0+IEFycmF5IGEgLT4gdmFsdWUpIC0+IERlY29kZXIgYSAtPiBEZWNvZGVyIHZhbHVlXG5vbmVPck1vcmUgdG9WYWx1ZSBkZWNvZGVyID1cbiAgICBhcnJheSBkZWNvZGVyXG4gICAgICAgIHw+IGFuZFRoZW4gKG9uZU9yTW9yZUhlbHAgdG9WYWx1ZSlcblxuXG5vbmVPck1vcmVIZWxwIDogKGEgLT4gQXJyYXkgYSAtPiB2YWx1ZSkgLT4gQXJyYXkgYSAtPiBEZWNvZGVyIHZhbHVlXG5vbmVPck1vcmVIZWxwIHRvVmFsdWUgeHMgPVxuICAgIGNhc2UgQXJyYXkuZ2V0IDAgeHMgb2ZcbiAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgZmFpbCBcImEgQVJSQVkgd2l0aCBhdCBsZWFzdCBPTkUgZWxlbWVudFwiXG5cbiAgICAgICAgSnVzdCB5IC0+XG4gICAgICAgICAgICBzdWNjZWVkICh0b1ZhbHVlIHkgKEFycmF5LnNsaWNlIDEgKEFycmF5Lmxlbmd0aCB4cykgeHMpKVxuXG5cblxuLS0gT0JKRUNUIFBSSU1JVElWRVNcblxuXG57LXwgRGVjb2RlIGEgSlNPTiBvYmplY3QsIHJlcXVpcmluZyBhIHBhcnRpY3VsYXIgZmllbGQuXG5cbiAgICBkZWNvZGVTdHJpbmcgKGZpZWxkIFwieFwiIGludCkgXCJ7IFxcXCJ4XFxcIjogMyB9XCIgPT0gT2sgM1xuXG4gICAgZGVjb2RlU3RyaW5nIChmaWVsZCBcInhcIiBpbnQpIFwieyBcXFwieFxcXCI6IDMsIFxcXCJ5XFxcIjogNCB9XCIgPT0gT2sgM1xuXG4gICAgZGVjb2RlU3RyaW5nIChmaWVsZCBcInhcIiBpbnQpIFwieyBcXFwieFxcXCI6IHRydWUgfVwiXG4gICAgICAgID09IEVyclxuICAgICAgICAuLi4gZGVjb2RlU3RyaW5nIChmaWVsZCBcInhcIiBpbnQpIFwieyBcXFwieVxcXCI6IDQgfVwiXG4gICAgICAgID09IEVyclxuICAgICAgICAuLi4gZGVjb2RlU3RyaW5nIChmaWVsZCBcIm5hbWVcIiBzdHJpbmcpIFwieyBcXFwibmFtZVxcXCI6IFxcXCJ0b21cXFwiIH1cIlxuICAgICAgICA9PSBPayBcInRvbVwiXG5cblRoZSBvYmplY3QgX2Nhbl8gaGF2ZSBvdGhlciBmaWVsZHMuIExvdHMgb2YgdGhlbSEgVGhlIG9ubHkgdGhpbmcgdGhpcyBkZWNvZGVyXG5jYXJlcyBhYm91dCBpcyBpZiBgeGAgaXMgcHJlc2VudCBhbmQgdGhhdCB0aGUgdmFsdWUgdGhlcmUgaXMgYW4gYEludGAuXG5cbkNoZWNrIG91dCBbYG1hcDJgXSgjbWFwMikgdG8gc2VlIGhvdyB0byBkZWNvZGUgbXVsdGlwbGUgZmllbGRzIVxuXG4tfVxuZmllbGQgOiBTdHJpbmcgLT4gRGVjb2RlciBhIC0+IERlY29kZXIgYVxuZmllbGQgPVxuICAgIEdyZW4uS2VybmVsLkpzb24uZGVjb2RlRmllbGRcblxuXG57LXwgRGVjb2RlIGEgbmVzdGVkIEpTT04gb2JqZWN0LCByZXF1aXJpbmcgY2VydGFpbiBmaWVsZHMuXG5cbiAgICBqc29uID0gXCJcIlwieyBcInBlcnNvblwiOiB7IFwibmFtZVwiOiBcInRvbVwiLCBcImFnZVwiOiA0MiB9IH1cIlwiXCJcblxuICAgIGRlY29kZVN0cmluZyAoYXQgW1wicGVyc29uXCIsIFwibmFtZVwiXSBzdHJpbmcpIGpzb24gID09IE9rIFwidG9tXCJcbiAgICBkZWNvZGVTdHJpbmcgKGF0IFtcInBlcnNvblwiLCBcImFnZVwiIF0gaW50ICAgKSBqc29uICA9PSBPayA0MlxuXG5UaGlzIGlzIHJlYWxseSBqdXN0IGEgc2hvcnRoYW5kIGZvciBzYXlpbmcgdGhpbmdzIGxpa2U6XG5cbiAgICBmaWVsZCBcInBlcnNvblwiIChmaWVsZCBcIm5hbWVcIiBzdHJpbmcpID09IGF0IFsgXCJwZXJzb25cIiwgXCJuYW1lXCIgXSBzdHJpbmdcblxuLX1cbmF0IDogQXJyYXkgU3RyaW5nIC0+IERlY29kZXIgYSAtPiBEZWNvZGVyIGFcbmF0IGZpZWxkcyBkZWNvZGVyID1cbiAgICBBcnJheS5mb2xkciBmaWVsZCBkZWNvZGVyIGZpZWxkc1xuXG5cbnstfCBEZWNvZGUgYSBKU09OIGFycmF5LCByZXF1aXJpbmcgYSBwYXJ0aWN1bGFyIGluZGV4LlxuXG4gICAganNvbiA9IFwiXCJcIlsgXCJhbGljZVwiLCBcImJvYlwiLCBcImNodWNrXCIgXVwiXCJcIlxuXG4gICAgZGVjb2RlU3RyaW5nIChpbmRleCAwIHN0cmluZykganNvbiAgPT0gT2sgXCJhbGljZVwiXG4gICAgZGVjb2RlU3RyaW5nIChpbmRleCAxIHN0cmluZykganNvbiAgPT0gT2sgXCJib2JcIlxuICAgIGRlY29kZVN0cmluZyAoaW5kZXggMiBzdHJpbmcpIGpzb24gID09IE9rIFwiY2h1Y2tcIlxuICAgIGRlY29kZVN0cmluZyAoaW5kZXggMyBzdHJpbmcpIGpzb24gID09IEVyciAuLi5cblxuLX1cbmluZGV4IDogSW50IC0+IERlY29kZXIgYSAtPiBEZWNvZGVyIGFcbmluZGV4ID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLmRlY29kZUluZGV4XG5cblxuXG4tLSBXRUlSRCBTVFJVQ1RVUkVcblxuXG57LXwgSGVscGZ1bCBmb3IgZGVhbGluZyB3aXRoIG9wdGlvbmFsIGZpZWxkcy4gSGVyZSBhcmUgYSBmZXcgc2xpZ2h0bHkgZGlmZmVyZW50XG5leGFtcGxlczpcblxuICAgIGpzb24gPSBcIlwiXCJ7IFwibmFtZVwiOiBcInRvbVwiLCBcImFnZVwiOiA0MiB9XCJcIlwiXG5cbiAgICBkZWNvZGVTdHJpbmcgKG1heWJlIChmaWVsZCBcImFnZVwiICAgIGludCAgKSkganNvbiA9PSBPayAoSnVzdCA0MilcbiAgICBkZWNvZGVTdHJpbmcgKG1heWJlIChmaWVsZCBcIm5hbWVcIiAgIGludCAgKSkganNvbiA9PSBPayBOb3RoaW5nXG4gICAgZGVjb2RlU3RyaW5nIChtYXliZSAoZmllbGQgXCJoZWlnaHRcIiBmbG9hdCkpIGpzb24gPT0gT2sgTm90aGluZ1xuXG4gICAgZGVjb2RlU3RyaW5nIChmaWVsZCBcImFnZVwiICAgIChtYXliZSBpbnQgICkpIGpzb24gPT0gT2sgKEp1c3QgNDIpXG4gICAgZGVjb2RlU3RyaW5nIChmaWVsZCBcIm5hbWVcIiAgIChtYXliZSBpbnQgICkpIGpzb24gPT0gT2sgTm90aGluZ1xuICAgIGRlY29kZVN0cmluZyAoZmllbGQgXCJoZWlnaHRcIiAobWF5YmUgZmxvYXQpKSBqc29uID09IEVyciAuLi5cblxuTm90aWNlIHRoZSBsYXN0IGV4YW1wbGUhIEl0IGlzIHNheWluZyB3ZSBfbXVzdF8gaGF2ZSBhIGZpZWxkIG5hbWVkIGBoZWlnaHRgIGFuZFxudGhlIGNvbnRlbnQgX21heV8gYmUgYSBmbG9hdC4gVGhlcmUgaXMgbm8gYGhlaWdodGAgZmllbGQsIHNvIHRoZSBkZWNvZGVyIGZhaWxzLlxuXG5Qb2ludCBpcywgYG1heWJlYCB3aWxsIG1ha2UgZXhhY3RseSB3aGF0IGl0IGNvbnRhaW5zIGNvbmRpdGlvbmFsLiBGb3Igb3B0aW9uYWxcbmZpZWxkcywgdGhpcyBtZWFucyB5b3UgcHJvYmFibHkgd2FudCBpdCBfb3V0c2lkZV8gYSB1c2Ugb2YgYGZpZWxkYCBvciBgYXRgLlxuXG4tfVxubWF5YmUgOiBEZWNvZGVyIGEgLT4gRGVjb2RlciAoTWF5YmUgYSlcbm1heWJlIGRlY29kZXIgPVxuICAgIG9uZU9mXG4gICAgICAgIFsgbWFwIEp1c3QgZGVjb2RlclxuICAgICAgICAsIHN1Y2NlZWQgTm90aGluZ1xuICAgICAgICBdXG5cblxuey18IFRyeSBhIGJ1bmNoIG9mIGRpZmZlcmVudCBkZWNvZGVycy4gVGhpcyBjYW4gYmUgdXNlZnVsIGlmIHRoZSBKU09OIG1heSBjb21lXG5pbiBhIGNvdXBsZSBkaWZmZXJlbnQgZm9ybWF0cy4gRm9yIGV4YW1wbGUsIHNheSB5b3Ugd2FudCB0byByZWFkIGFuIGFycmF5IG9mXG5udW1iZXJzLCBidXQgc29tZSBvZiB0aGVtIGFyZSBgbnVsbGAuXG5cbiAgICBpbXBvcnQgU3RyaW5nXG5cbiAgICBiYWRJbnQgOiBEZWNvZGVyIEludFxuICAgIGJhZEludCA9XG4gICAgICAgIG9uZU9mIFsgaW50LCBudWxsIDAgXVxuXG4gICAgLS0gZGVjb2RlU3RyaW5nIChhcnJheSBiYWRJbnQpIFwiWzEsMixudWxsLDRdXCIgPT0gT2sgWzEsMiwwLDRdXG5cbldoeSB3b3VsZCBzb21lb25lIGdlbmVyYXRlIEpTT04gbGlrZSB0aGlzPyBRdWVzdGlvbnMgbGlrZSB0aGlzIGFyZSBub3QgZ29vZFxuZm9yIHlvdXIgaGVhbHRoLiBUaGUgcG9pbnQgaXMgdGhhdCB5b3UgY2FuIHVzZSBgb25lT2ZgIHRvIGhhbmRsZSBzaXR1YXRpb25zXG5saWtlIHRoaXMhXG5cbllvdSBjb3VsZCBhbHNvIHVzZSBgb25lT2ZgIHRvIGhlbHAgdmVyc2lvbiB5b3VyIGRhdGEuIFRyeSB0aGUgbGF0ZXN0IGZvcm1hdCxcbnRoZW4gYSBmZXcgb2xkZXIgb25lcyB0aGF0IHlvdSBzdGlsbCBzdXBwb3J0LiBZb3UgY291bGQgdXNlIGBhbmRUaGVuYCB0byBiZVxuZXZlbiBtb3JlIHBhcnRpY3VsYXIgaWYgeW91IHdhbnRlZC5cblxuLX1cbm9uZU9mIDogQXJyYXkgKERlY29kZXIgYSkgLT4gRGVjb2RlciBhXG5vbmVPZiA9XG4gICAgR3Jlbi5LZXJuZWwuSnNvbi5vbmVPZlxuXG5cblxuLS0gTUFQUElOR1xuXG5cbnstfCBUcmFuc2Zvcm0gYSBkZWNvZGVyLiBNYXliZSB5b3UganVzdCB3YW50IHRvIGtub3cgdGhlIGxlbmd0aCBvZiBhIHN0cmluZzpcblxuICAgIGltcG9ydCBTdHJpbmdcblxuICAgIHN0cmluZ0xlbmd0aCA6IERlY29kZXIgSW50XG4gICAgc3RyaW5nTGVuZ3RoID1cbiAgICAgICAgbWFwIFN0cmluZy5sZW5ndGggc3RyaW5nXG5cbkl0IGlzIG9mdGVuIGhlbHBmdWwgdG8gdXNlIGBtYXBgIHdpdGggYG9uZU9mYCwgbGlrZSB3aGVuIGRlZmluaW5nIGBudWxsYWJsZWA6XG5cbiAgICBudWxsYWJsZSA6IERlY29kZXIgYSAtPiBEZWNvZGVyIChNYXliZSBhKVxuICAgIG51bGxhYmxlIGRlY29kZXIgPVxuICAgICAgICBvbmVPZlxuICAgICAgICAgICAgWyBudWxsIE5vdGhpbmdcbiAgICAgICAgICAgICwgbWFwIEp1c3QgZGVjb2RlclxuICAgICAgICAgICAgXVxuXG4tfVxubWFwIDogKGEgLT4gdmFsdWUpIC0+IERlY29kZXIgYSAtPiBEZWNvZGVyIHZhbHVlXG5tYXAgPVxuICAgIEdyZW4uS2VybmVsLkpzb24ubWFwMVxuXG5cbnstfCBUcnkgdHdvIGRlY29kZXJzIGFuZCB0aGVuIGNvbWJpbmUgdGhlIHJlc3VsdC4gV2UgY2FuIHVzZSB0aGlzIHRvIGRlY29kZVxub2JqZWN0cyB3aXRoIG1hbnkgZmllbGRzOlxuXG5cbiAgICB0eXBlIGFsaWFzIFBvaW50ID1cbiAgICAgICAgeyB4IDogRmxvYXRcbiAgICAgICAgLCB5IDogRmxvYXRcbiAgICAgICAgfVxuXG4gICAgbWFrZVBvaW50IDogRmxvYXQgLT4gRmxvYXQgLT4gUG9pbnRcbiAgICBtYWtlUG9pbnQgeCB5ID1cbiAgICAgICAgeyB4ID0geFxuICAgICAgICAsIHkgPSB5XG4gICAgICAgIH1cblxuICAgIHBvaW50IDogRGVjb2RlciBQb2ludFxuICAgIHBvaW50ID1cbiAgICAgICAgbWFwMiBtYWtlUG9pbnQgKGZpZWxkIFwieFwiIGZsb2F0KSAoZmllbGQgXCJ5XCIgZmxvYXQpXG5cbiAgICAtLSBkZWNvZGVTdHJpbmcgcG9pbnQgXCJcIlwieyBcInhcIjogMywgXCJ5XCI6IDQgfVwiXCJcIiA9PSBPayB7IHggPSAzLCB5ID0gNCB9XG5cbkl0IHRyaWVzIGVhY2ggaW5kaXZpZHVhbCBkZWNvZGVyIGFuZCBwdXRzIHRoZSByZXN1bHQgdG9nZXRoZXIgd2l0aCB0aGUgYFBvaW50YFxuY29uc3RydWN0b3IuXG5cbi19XG5tYXAyIDogKGEgLT4gYiAtPiB2YWx1ZSkgLT4gRGVjb2RlciBhIC0+IERlY29kZXIgYiAtPiBEZWNvZGVyIHZhbHVlXG5tYXAyID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLm1hcDJcblxuXG57LXwgVHJ5IHRocmVlIGRlY29kZXJzIGFuZCB0aGVuIGNvbWJpbmUgdGhlIHJlc3VsdC4gV2UgY2FuIHVzZSB0aGlzIHRvIGRlY29kZVxub2JqZWN0cyB3aXRoIG1hbnkgZmllbGRzOlxuXG5cbiAgICB0eXBlIGFsaWFzIFBlcnNvbiA9XG4gICAgICAgIHsgbmFtZSA6IFN0cmluZywgYWdlIDogSW50LCBoZWlnaHQgOiBGbG9hdCB9XG5cbiAgICBtYWtlUGVyc29uIDogU3RyaW5nIC0+IEludCAtPiBGbG9hdCAtPiBQZXJzb25cbiAgICBtYWtlUGVyc29uIG5hbWUgYWdlIGhlaWdodCA9XG4gICAgICAgIHsgbmFtZSA9IG5hbWVcbiAgICAgICAgLCBhZ2UgPSBhZ2VcbiAgICAgICAgLCBoZWlnaHQgPSBoZWlnaHRcbiAgICAgICAgfVxuXG4gICAgcGVyc29uIDogRGVjb2RlciBQZXJzb25cbiAgICBwZXJzb24gPVxuICAgICAgICBtYXAzIG1ha2VQZXJzb25cbiAgICAgICAgICAgIChhdCBbIFwibmFtZVwiIF0gc3RyaW5nKVxuICAgICAgICAgICAgKGF0IFsgXCJpbmZvXCIsIFwiYWdlXCIgXSBpbnQpXG4gICAgICAgICAgICAoYXQgWyBcImluZm9cIiwgXCJoZWlnaHRcIiBdIGZsb2F0KVxuXG4gICAgLS0ganNvbiA9IFwiXCJcInsgXCJuYW1lXCI6IFwidG9tXCIsIFwiaW5mb1wiOiB7IFwiYWdlXCI6IDQyLCBcImhlaWdodFwiOiAxLjggfSB9XCJcIlwiXG4gICAgLS0gZGVjb2RlU3RyaW5nIHBlcnNvbiBqc29uID09IE9rIHsgbmFtZSA9IFwidG9tXCIsIGFnZSA9IDQyLCBoZWlnaHQgPSAxLjggfVxuXG5MaWtlIGBtYXAyYCBpdCB0cmllcyBlYWNoIGRlY29kZXIgaW4gb3JkZXIgYW5kIHRoZW4gZ2l2ZSB0aGUgcmVzdWx0cyB0byB0aGVcbmBQZXJzb25gIGNvbnN0cnVjdG9yLiBUaGF0IGNhbiBiZSBhbnkgZnVuY3Rpb24gdGhvdWdoIVxuXG4tfVxubWFwMyA6IChhIC0+IGIgLT4gYyAtPiB2YWx1ZSkgLT4gRGVjb2RlciBhIC0+IERlY29kZXIgYiAtPiBEZWNvZGVyIGMgLT4gRGVjb2RlciB2YWx1ZVxubWFwMyA9XG4gICAgR3Jlbi5LZXJuZWwuSnNvbi5tYXAzXG5cblxuey18IC19XG5tYXA0IDogKGEgLT4gYiAtPiBjIC0+IGQgLT4gdmFsdWUpIC0+IERlY29kZXIgYSAtPiBEZWNvZGVyIGIgLT4gRGVjb2RlciBjIC0+IERlY29kZXIgZCAtPiBEZWNvZGVyIHZhbHVlXG5tYXA0ID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLm1hcDRcblxuXG57LXwgLX1cbm1hcDUgOiAoYSAtPiBiIC0+IGMgLT4gZCAtPiBlIC0+IHZhbHVlKSAtPiBEZWNvZGVyIGEgLT4gRGVjb2RlciBiIC0+IERlY29kZXIgYyAtPiBEZWNvZGVyIGQgLT4gRGVjb2RlciBlIC0+IERlY29kZXIgdmFsdWVcbm1hcDUgPVxuICAgIEdyZW4uS2VybmVsLkpzb24ubWFwNVxuXG5cbnstfCAtfVxubWFwNiA6IChhIC0+IGIgLT4gYyAtPiBkIC0+IGUgLT4gZiAtPiB2YWx1ZSkgLT4gRGVjb2RlciBhIC0+IERlY29kZXIgYiAtPiBEZWNvZGVyIGMgLT4gRGVjb2RlciBkIC0+IERlY29kZXIgZSAtPiBEZWNvZGVyIGYgLT4gRGVjb2RlciB2YWx1ZVxubWFwNiA9XG4gICAgR3Jlbi5LZXJuZWwuSnNvbi5tYXA2XG5cblxuey18IC19XG5tYXA3IDogKGEgLT4gYiAtPiBjIC0+IGQgLT4gZSAtPiBmIC0+IGcgLT4gdmFsdWUpIC0+IERlY29kZXIgYSAtPiBEZWNvZGVyIGIgLT4gRGVjb2RlciBjIC0+IERlY29kZXIgZCAtPiBEZWNvZGVyIGUgLT4gRGVjb2RlciBmIC0+IERlY29kZXIgZyAtPiBEZWNvZGVyIHZhbHVlXG5tYXA3ID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLm1hcDdcblxuXG57LXwgLX1cbm1hcDggOiAoYSAtPiBiIC0+IGMgLT4gZCAtPiBlIC0+IGYgLT4gZyAtPiBoIC0+IHZhbHVlKSAtPiBEZWNvZGVyIGEgLT4gRGVjb2RlciBiIC0+IERlY29kZXIgYyAtPiBEZWNvZGVyIGQgLT4gRGVjb2RlciBlIC0+IERlY29kZXIgZiAtPiBEZWNvZGVyIGcgLT4gRGVjb2RlciBoIC0+IERlY29kZXIgdmFsdWVcbm1hcDggPVxuICAgIEdyZW4uS2VybmVsLkpzb24ubWFwOFxuXG5cblxuLS0gUlVOIERFQ09ERVJTXG5cblxuey18IFBhcnNlIHRoZSBnaXZlbiBzdHJpbmcgaW50byBhIEpTT04gdmFsdWUgYW5kIHRoZW4gcnVuIHRoZSBgRGVjb2RlcmAgb24gaXQuXG5UaGlzIHdpbGwgZmFpbCBpZiB0aGUgc3RyaW5nIGlzIG5vdCB3ZWxsLWZvcm1lZCBKU09OIG9yIGlmIHRoZSBgRGVjb2RlcmBcbmZhaWxzIGZvciBzb21lIHJlYXNvbi5cblxuICAgIGRlY29kZVN0cmluZyBpbnQgXCI0XCIgICAgID09IE9rIDRcbiAgICBkZWNvZGVTdHJpbmcgaW50IFwiMSArIDJcIiA9PSBFcnIgLi4uXG5cbi19XG5kZWNvZGVTdHJpbmcgOiBEZWNvZGVyIGEgLT4gU3RyaW5nIC0+IFJlc3VsdCBFcnJvciBhXG5kZWNvZGVTdHJpbmcgPVxuICAgIEdyZW4uS2VybmVsLkpzb24ucnVuT25TdHJpbmdcblxuXG57LXwgUnVuIGEgYERlY29kZXJgIG9uIHNvbWUgSlNPTiBgVmFsdWVgLiBZb3UgY2FuIHNlbmQgdGhlc2UgSlNPTiB2YWx1ZXNcbnRocm91Z2ggcG9ydHMsIHNvIHRoYXQgaXMgcHJvYmFibHkgdGhlIG1haW4gdGltZSB5b3Ugd291bGQgdXNlIHRoaXMgZnVuY3Rpb24uXG4tfVxuZGVjb2RlVmFsdWUgOiBEZWNvZGVyIGEgLT4gVmFsdWUgLT4gUmVzdWx0IEVycm9yIGFcbmRlY29kZVZhbHVlID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLnJ1blxuXG5cbnstfCBSZXByZXNlbnRzIGEgSmF2YVNjcmlwdCB2YWx1ZS5cbi19XG50eXBlIGFsaWFzIFZhbHVlID1cbiAgICBKc29uLkVuY29kZS5WYWx1ZVxuXG5cbnstfCBBIHN0cnVjdHVyZWQgZXJyb3IgZGVzY3JpYmluZyBleGFjdGx5IGhvdyB0aGUgZGVjb2RlciBmYWlsZWQuIFlvdSBjYW4gdXNlXG50aGlzIHRvIGNyZWF0ZSBtb3JlIGVsYWJvcmF0ZSB2aXN1YWxpemF0aW9ucyBvZiBhIGRlY29kZXIgcHJvYmxlbS4gRm9yIGV4YW1wbGUsXG55b3UgY291bGQgc2hvdyB0aGUgZW50aXJlIEpTT04gb2JqZWN0IGFuZCBzaG93IHRoZSBwYXJ0IGNhdXNpbmcgdGhlIGZhaWx1cmUgaW5cbnJlZC5cbi19XG50eXBlIEVycm9yXG4gICAgPSBGaWVsZCBTdHJpbmcgRXJyb3JcbiAgICB8IEluZGV4IEludCBFcnJvclxuICAgIHwgT25lT2YgKEFycmF5IEVycm9yKVxuICAgIHwgRmFpbHVyZSBTdHJpbmcgVmFsdWVcblxuXG57LXwgQ29udmVydCBhIGRlY29kaW5nIGVycm9yIGludG8gYSBgU3RyaW5nYCB0aGF0IGlzIG5pY2UgZm9yIGRlYnVnZ2luZy5cblxuSXQgcHJvZHVjZXMgbXVsdGlwbGUgbGluZXMgb2Ygb3V0cHV0LCBzbyB5b3UgbWF5IHdhbnQgdG8gcGVlayBhdCBpdCB3aXRoXG5zb21ldGhpbmcgbGlrZSB0aGlzOlxuXG4gICAgaW1wb3J0IEh0bWxcbiAgICBpbXBvcnQgSnNvbi5EZWNvZGUgYXMgRGVjb2RlXG5cbiAgICBlcnJvclRvSHRtbCA6IERlY29kZS5FcnJvciAtPiBIdG1sLkh0bWwgbXNnXG4gICAgZXJyb3JUb0h0bWwgZXJyb3IgPVxuICAgICAgICBIdG1sLnByZSBbXSBbIEh0bWwudGV4dCAoRGVjb2RlLmVycm9yVG9TdHJpbmcgZXJyb3IpIF1cblxuKipOb3RlOioqIEl0IHdvdWxkIGJlIGNvb2wgdG8gZG8gbmljZXIgY29sb3JpbmcgYW5kIGZhbmNpZXIgSFRNTCwgYnV0IEkgd2FudGVkXG50byBhdm9pZCBoYXZpbmcgYW4gYGVsbS9odG1sYCBkZXBlbmRlbmN5IGZvciBub3cuIEl0IGlzIHRvdGFsbHkgcG9zc2libGUgdG9cbmNyYXdsIHRoZSBgRXJyb3JgIHN0cnVjdHVyZSBhbmQgY3JlYXRlIHRoaXMgc2VwYXJhdGVseSB0aG91Z2ghXG5cbi19XG5lcnJvclRvU3RyaW5nIDogRXJyb3IgLT4gU3RyaW5nXG5lcnJvclRvU3RyaW5nIGVycm9yID1cbiAgICBlcnJvclRvU3RyaW5nSGVscCBlcnJvciBbXVxuXG5cbmVycm9yVG9TdHJpbmdIZWxwIDogRXJyb3IgLT4gQXJyYXkgU3RyaW5nIC0+IFN0cmluZ1xuZXJyb3JUb1N0cmluZ0hlbHAgZXJyb3IgY29udGV4dCA9XG4gICAgY2FzZSBlcnJvciBvZlxuICAgICAgICBGaWVsZCBmIGVyciAtPlxuICAgICAgICAgICAgbGV0XG4gICAgICAgICAgICAgICAgaXNTaW1wbGUgPVxuICAgICAgICAgICAgICAgICAgICBjYXNlIFN0cmluZy51bmNvbnMgZiBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEZhbHNlXG5cbiAgICAgICAgICAgICAgICAgICAgICAgIEp1c3QgeyBmaXJzdCA9IGNoYXIsIHJlc3QgfSAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIENoYXIuaXNBbHBoYSBjaGFyICYmIFN0cmluZy5hbGwgQ2hhci5pc0FscGhhTnVtIHJlc3RcblxuICAgICAgICAgICAgICAgIGZpZWxkTmFtZSA9XG4gICAgICAgICAgICAgICAgICAgIGlmIGlzU2ltcGxlIHRoZW5cbiAgICAgICAgICAgICAgICAgICAgICAgIFwiLlwiICsrIGZcblxuICAgICAgICAgICAgICAgICAgICBlbHNlXG4gICAgICAgICAgICAgICAgICAgICAgICBcIlsnXCIgKysgZiArKyBcIiddXCJcbiAgICAgICAgICAgIGluXG4gICAgICAgICAgICBlcnJvclRvU3RyaW5nSGVscCBlcnIgKFsgZmllbGROYW1lIF0gKysgY29udGV4dClcblxuICAgICAgICBJbmRleCBpIGVyciAtPlxuICAgICAgICAgICAgbGV0XG4gICAgICAgICAgICAgICAgaW5kZXhOYW1lID1cbiAgICAgICAgICAgICAgICAgICAgXCJbXCIgKysgU3RyaW5nLmZyb21JbnQgaSArKyBcIl1cIlxuICAgICAgICAgICAgaW5cbiAgICAgICAgICAgIGVycm9yVG9TdHJpbmdIZWxwIGVyciAoWyBpbmRleE5hbWUgXSArKyBjb250ZXh0KVxuXG4gICAgICAgIE9uZU9mIGVycm9ycyAtPlxuICAgICAgICAgICAgY2FzZSBlcnJvcnMgb2ZcbiAgICAgICAgICAgICAgICBbXSAtPlxuICAgICAgICAgICAgICAgICAgICBcIlJhbiBpbnRvIGEgSnNvbi5EZWNvZGUub25lT2Ygd2l0aCBubyBwb3NzaWJpbGl0aWVzXCJcbiAgICAgICAgICAgICAgICAgICAgICAgICsrIChjYXNlIGNvbnRleHQgb2ZcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgW10gLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiIVwiXG5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCIgYXQganNvblwiICsrIFN0cmluZy5qb2luIFwiXCIgY29udGV4dFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgKVxuXG4gICAgICAgICAgICAgICAgWyBlcnIgXSAtPlxuICAgICAgICAgICAgICAgICAgICBlcnJvclRvU3RyaW5nSGVscCBlcnIgY29udGV4dFxuXG4gICAgICAgICAgICAgICAgXyAtPlxuICAgICAgICAgICAgICAgICAgICBsZXRcbiAgICAgICAgICAgICAgICAgICAgICAgIHN0YXJ0ZXIgPVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNhc2UgY29udGV4dCBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBbXSAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJKc29uLkRlY29kZS5vbmVPZlwiXG5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJUaGUgSnNvbi5EZWNvZGUub25lT2YgYXQganNvblwiICsrIFN0cmluZy5qb2luIFwiXCIgY29udGV4dFxuXG4gICAgICAgICAgICAgICAgICAgICAgICBpbnRyb2R1Y3Rpb24gPVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHN0YXJ0ZXIgKysgXCIgZmFpbGVkIGluIHRoZSBmb2xsb3dpbmcgXCIgKysgU3RyaW5nLmZyb21JbnQgKEFycmF5Lmxlbmd0aCBlcnJvcnMpICsrIFwiIHdheXM6XCJcbiAgICAgICAgICAgICAgICAgICAgaW5cbiAgICAgICAgICAgICAgICAgICAgU3RyaW5nLmpvaW4gXCJcXG5cXG5cIiAoWyBpbnRyb2R1Y3Rpb24gXSArKyBBcnJheS5pbmRleGVkTWFwIGVycm9yT25lT2YgZXJyb3JzKVxuXG4gICAgICAgIEZhaWx1cmUgbXNnIGpzb24gLT5cbiAgICAgICAgICAgIGxldFxuICAgICAgICAgICAgICAgIGludHJvZHVjdGlvbiA9XG4gICAgICAgICAgICAgICAgICAgIGNhc2UgY29udGV4dCBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgW10gLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIlByb2JsZW0gd2l0aCB0aGUgZ2l2ZW4gdmFsdWU6XFxuXFxuXCJcblxuICAgICAgICAgICAgICAgICAgICAgICAgXyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiUHJvYmxlbSB3aXRoIHRoZSB2YWx1ZSBhdCBqc29uXCIgKysgU3RyaW5nLmpvaW4gXCJcIiBjb250ZXh0ICsrIFwiOlxcblxcbiAgICBcIlxuICAgICAgICAgICAgaW5cbiAgICAgICAgICAgIGludHJvZHVjdGlvbiArKyBpbmRlbnQgKEpzb24uRW5jb2RlLmVuY29kZSA0IGpzb24pICsrIFwiXFxuXFxuXCIgKysgbXNnXG5cblxuZXJyb3JPbmVPZiA6IEludCAtPiBFcnJvciAtPiBTdHJpbmdcbmVycm9yT25lT2YgaSBlcnJvciA9XG4gICAgXCJcXG5cXG4oXCIgKysgU3RyaW5nLmZyb21JbnQgKGkgKyAxKSArKyBcIikgXCIgKysgaW5kZW50IChlcnJvclRvU3RyaW5nIGVycm9yKVxuXG5cbmluZGVudCA6IFN0cmluZyAtPiBTdHJpbmdcbmluZGVudCBzdHIgPVxuICAgIFN0cmluZy5qb2luIFwiXFxuICAgIFwiIChTdHJpbmcuc3BsaXQgXCJcXG5cIiBzdHIpXG5cblxuXG4tLSBGQU5DWSBQUklNSVRJVkVTXG5cblxuey18IElnbm9yZSB0aGUgSlNPTiBhbmQgcHJvZHVjZSBhIGNlcnRhaW4gR3JlbiB2YWx1ZS5cblxuICAgIGRlY29kZVN0cmluZyAoc3VjY2VlZCA0MikgXCJ0cnVlXCIgICAgPT0gT2sgNDJcbiAgICBkZWNvZGVTdHJpbmcgKHN1Y2NlZWQgNDIpIFwiWzEsMiwzXVwiID09IE9rIDQyXG4gICAgZGVjb2RlU3RyaW5nIChzdWNjZWVkIDQyKSBcImhlbGxvXCIgICA9PSBFcnIgLi4uIC0tIHRoaXMgaXMgbm90IGEgdmFsaWQgSlNPTiBzdHJpbmdcblxuVGhpcyBpcyBoYW5keSB3aGVuIHVzZWQgd2l0aCBgb25lT2ZgIG9yIGBhbmRUaGVuYC5cblxuLX1cbnN1Y2NlZWQgOiBhIC0+IERlY29kZXIgYVxuc3VjY2VlZCA9XG4gICAgR3Jlbi5LZXJuZWwuSnNvbi5zdWNjZWVkXG5cblxuey18IElnbm9yZSB0aGUgSlNPTiBhbmQgbWFrZSB0aGUgZGVjb2RlciBmYWlsLiBUaGlzIGlzIGhhbmR5IHdoZW4gdXNlZCB3aXRoXG5gb25lT2ZgIG9yIGBhbmRUaGVuYCB3aGVyZSB5b3Ugd2FudCB0byBnaXZlIGEgY3VzdG9tIGVycm9yIG1lc3NhZ2UgaW4gc29tZVxuY2FzZS5cblxuU2VlIHRoZSBbYGFuZFRoZW5gXSgjYW5kVGhlbikgZG9jcyBmb3IgYW4gZXhhbXBsZS5cblxuLX1cbmZhaWwgOiBTdHJpbmcgLT4gRGVjb2RlciBhXG5mYWlsID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLmZhaWxcblxuXG57LXwgQ3JlYXRlIGRlY29kZXJzIHRoYXQgZGVwZW5kIG9uIHByZXZpb3VzIHJlc3VsdHMuIElmIHlvdSBhcmUgY3JlYXRpbmdcbnZlcnNpb25lZCBkYXRhLCB5b3UgbWlnaHQgZG8gc29tZXRoaW5nIGxpa2UgdGhpczpcblxuXG4gICAgaW5mbyA6IERlY29kZXIgSW5mb1xuICAgIGluZm8gPVxuICAgICAgICBmaWVsZCBcInZlcnNpb25cIiBpbnRcbiAgICAgICAgICAgIHw+IGFuZFRoZW4gaW5mb0hlbHBcblxuICAgIGluZm9IZWxwIDogSW50IC0+IERlY29kZXIgSW5mb1xuICAgIGluZm9IZWxwIHZlcnNpb24gPVxuICAgICAgICBjYXNlIHZlcnNpb24gb2ZcbiAgICAgICAgICAgIDQgLT5cbiAgICAgICAgICAgICAgICBpbmZvRGVjb2RlcjRcblxuICAgICAgICAgICAgMyAtPlxuICAgICAgICAgICAgICAgIGluZm9EZWNvZGVyM1xuXG4gICAgICAgICAgICBfIC0+XG4gICAgICAgICAgICAgICAgZmFpbCA8fFxuICAgICAgICAgICAgICAgICAgICBcIlRyeWluZyB0byBkZWNvZGUgaW5mbywgYnV0IHZlcnNpb24gXCJcbiAgICAgICAgICAgICAgICAgICAgICAgICsrIHRvU3RyaW5nIHZlcnNpb25cbiAgICAgICAgICAgICAgICAgICAgICAgICsrIFwiIGlzIG5vdCBzdXBwb3J0ZWQuXCJcblxuICAgIC0tIGluZm9EZWNvZGVyNCA6IERlY29kZXIgSW5mb1xuICAgIC0tIGluZm9EZWNvZGVyMyA6IERlY29kZXIgSW5mb1xuXG4tfVxuYW5kVGhlbiA6IChhIC0+IERlY29kZXIgYikgLT4gRGVjb2RlciBhIC0+IERlY29kZXIgYlxuYW5kVGhlbiA9XG4gICAgR3Jlbi5LZXJuZWwuSnNvbi5hbmRUaGVuXG5cblxuey18IFNvbWV0aW1lcyB5b3UgaGF2ZSBKU09OIHdpdGggcmVjdXJzaXZlIHN0cnVjdHVyZSwgbGlrZSBuZXN0ZWQgY29tbWVudHMuXG5Zb3UgY2FuIHVzZSBgbGF6eWAgdG8gbWFrZSBzdXJlIHlvdXIgZGVjb2RlciB1bnJvbGxzIGxhemlseS5cblxuICAgIHR5cGUgYWxpYXMgQ29tbWVudCA9XG4gICAgICAgIHsgbWVzc2FnZSA6IFN0cmluZ1xuICAgICAgICAsIHJlc3BvbnNlcyA6IFJlc3BvbnNlc1xuICAgICAgICB9XG5cbiAgICBtYWtlQ29tbWVudCA6IFN0cmluZyAtPiBSZXNwb25zZXMgLT4gQ29tbWVudFxuICAgIG1ha2VDb21tZW50IG1lc3NhZ2UgcmVzcG9uc2VzID1cbiAgICAgICAgeyBtZXNzYWdlID0gbWVzc2FnZVxuICAgICAgICAsIHJlc3BvbnNlcyA9IHJlc3BvbnNlc1xuICAgICAgICB9XG5cbiAgICB0eXBlIFJlc3BvbnNlc1xuICAgICAgICA9IFJlc3BvbnNlcyAoQXJyYXkgQ29tbWVudClcblxuICAgIGNvbW1lbnQgOiBEZWNvZGVyIENvbW1lbnRcbiAgICBjb21tZW50ID1cbiAgICAgICAgbWFwMiBtYWtlQ29tbWVudFxuICAgICAgICAgICAgKGZpZWxkIFwibWVzc2FnZVwiIHN0cmluZylcbiAgICAgICAgICAgIChmaWVsZCBcInJlc3BvbnNlc1wiIChtYXAgUmVzcG9uc2VzIChhcnJheSAobGF6eSAoXFxfIC0+IGNvbW1lbnQpKSkpKVxuXG5JZiB3ZSBoYWQgc2FpZCBgYXJyYXkgY29tbWVudGAgaW5zdGVhZCwgd2Ugd291bGQgc3RhcnQgZXhwYW5kaW5nIHRoZSB2YWx1ZVxuaW5maW5pdGVseS4gV2hhdCBpcyBhIGBjb21tZW50YD8gSXQgaXMgYSBkZWNvZGVyIGZvciBvYmplY3RzIHdoZXJlIHRoZVxuYHJlc3BvbnNlc2AgZmllbGQgY29udGFpbnMgY29tbWVudHMuIFdoYXQgaXMgYSBgY29tbWVudGAgdGhvdWdoPyBFdGMuXG5cbkJ5IHVzaW5nIGBhcnJheSAobGF6eSAoXFxfIC0+IGNvbW1lbnQpKWAgd2UgbWFrZSBzdXJlIHRoZSBkZWNvZGVyIG9ubHkgZXhwYW5kc1xudG8gYmUgYXMgZGVlcCBhcyB0aGUgSlNPTiB3ZSBhcmUgZ2l2ZW4uIFlvdSBjYW4gcmVhZCBtb3JlIGFib3V0IHJlY3Vyc2l2ZSBkYXRhXG5zdHJ1Y3R1cmVzIFtoZXJlXS5cblxuW2hlcmVdOiBodHRwczovL2dpdGh1Yi5jb20vZWxtL2NvbXBpbGVyL2Jsb2IvbWFzdGVyL2hpbnRzL3JlY3Vyc2l2ZS1hbGlhcy5tZFxuXG4tfVxubGF6eSA6ICh7fSAtPiBEZWNvZGVyIGEpIC0+IERlY29kZXIgYVxubGF6eSB0aHVuayA9XG4gICAgYW5kVGhlbiB0aHVuayAoc3VjY2VlZCB7fSlcblxuXG57LXwgRG8gbm90IGRvIGFueXRoaW5nIHdpdGggYSBKU09OIHZhbHVlLCBqdXN0IGJyaW5nIGl0IGludG8gR3JlbiBhcyBhIGBWYWx1ZWAuXG5UaGlzIGNhbiBiZSB1c2VmdWwgaWYgeW91IGhhdmUgcGFydGljdWxhcmx5IGNvbXBsZXggZGF0YSB0aGF0IHlvdSB3b3VsZCBsaWtlIHRvXG5kZWFsIHdpdGggbGF0ZXIuIE9yIGlmIHlvdSBhcmUgZ29pbmcgdG8gc2VuZCBpdCBvdXQgYSBwb3J0IGFuZCBkbyBub3QgY2FyZVxuYWJvdXQgaXRzIHN0cnVjdHVyZS5cbi19XG52YWx1ZSA6IERlY29kZXIgVmFsdWVcbnZhbHVlID1cbiAgICBHcmVuLktlcm5lbC5Kc29uLmRlY29kZVZhbHVlXG5cblxuey18IERlY29kZSBhIGBudWxsYCB2YWx1ZSBpbnRvIHNvbWUgR3JlbiB2YWx1ZS5cblxuICAgIGRlY29kZVN0cmluZyAobnVsbCBGYWxzZSkgXCJudWxsXCIgPT0gT2sgRmFsc2VcbiAgICBkZWNvZGVTdHJpbmcgKG51bGwgNDIpIFwibnVsbFwiICAgID09IE9rIDQyXG4gICAgZGVjb2RlU3RyaW5nIChudWxsIDQyKSBcIjQyXCIgICAgICA9PSBFcnIgLi5cbiAgICBkZWNvZGVTdHJpbmcgKG51bGwgNDIpIFwiZmFsc2VcIiAgID09IEVyciAuLlxuXG5TbyBpZiB5b3UgZXZlciBzZWUgYSBgbnVsbGAsIHRoaXMgd2lsbCByZXR1cm4gd2hhdGV2ZXIgdmFsdWUgeW91IHNwZWNpZmllZC5cblxuLX1cbm51bGwgOiBhIC0+IERlY29kZXIgYVxubnVsbCA9XG4gICAgR3Jlbi5LZXJuZWwuSnNvbi5kZWNvZGVOdWxsXG4iLAogICAgICAgICJtb2R1bGUgQ2hhciBleHBvc2luZ1xuICAgICggQ2hhclxuICAgICwgaXNVcHBlciwgaXNMb3dlciwgaXNBbHBoYSwgaXNBbHBoYU51bVxuICAgICwgaXNEaWdpdCwgaXNPY3REaWdpdCwgaXNIZXhEaWdpdFxuICAgICwgdG9VcHBlciwgdG9Mb3dlciwgdG9Mb2NhbGVVcHBlciwgdG9Mb2NhbGVMb3dlclxuICAgICwgdG9Db2RlLCBmcm9tQ29kZVxuICAgIClcblxuey18IEZ1bmN0aW9ucyBmb3Igd29ya2luZyB3aXRoIGNoYXJhY3RlcnMuIENoYXJhY3RlciBsaXRlcmFscyBhcmUgZW5jbG9zZWQgaW5cbmAnYSdgIHBhaXIgb2Ygc2luZ2xlIHF1b3Rlcy5cblxuXG5AZG9jcyBDaGFyXG5cblxuIyMgQVNDSUkgTGV0dGVyc1xuXG5AZG9jcyBpc1VwcGVyLCBpc0xvd2VyLCBpc0FscGhhLCBpc0FscGhhTnVtXG5cblxuIyMgRGlnaXRzXG5cbkBkb2NzIGlzRGlnaXQsIGlzT2N0RGlnaXQsIGlzSGV4RGlnaXRcblxuXG4jIyBDb252ZXJzaW9uXG5cbkBkb2NzIHRvVXBwZXIsIHRvTG93ZXIsIHRvTG9jYWxlVXBwZXIsIHRvTG9jYWxlTG93ZXJcblxuXG4jIyBVbmljb2RlIENvZGUgUG9pbnRzXG5cbkBkb2NzIHRvQ29kZSwgZnJvbUNvZGVcblxuLX1cblxuaW1wb3J0IEJhc2ljcyBleHBvc2luZyAoKCYmKSwgKDw9KSwgKD49KSwgKHx8KSwgQm9vbCwgSW50KVxuaW1wb3J0IEdyZW4uS2VybmVsLkNoYXJcblxuXG5cbi0tIENIQVJcblxuXG57LXwgQSBgQ2hhcmAgaXMgYSBzaW5nbGUgW3VuaWNvZGVdW3VdIGNoYXJhY3RlcjpcblxuICAgICdhJ1xuXG4gICAgJzAnXG5cbiAgICAnWidcblxuICAgICc/J1xuXG4gICAgJ1wiJ1xuXG4gICAgJ86jJ1xuXG4gICAgJ/CfmYgnXG5cbiAgICAnXFx0J1xuXG4gICAgJ1wiJ1xuXG4gICAgJ1xcJydcblxuICAgICfwn5mIJyAtLSAn8J+ZiCdcblxuKipOb3RlIDE6KiogWW91IF9jYW5ub3RfIHVzZSBzaW5nbGUgcXVvdGVzIGFyb3VuZCBtdWx0aXBsZSBjaGFyYWN0ZXJzIGxpa2UgaW5cbkphdmFTY3JpcHQuIFRoaXMgaXMgaG93IHdlIGRpc3Rpbmd1aXNoIFtgU3RyaW5nYF0oU3RyaW5nI1N0cmluZykgYW5kIGBDaGFyYFxudmFsdWVzIGluIHN5bnRheC5cblxuKipOb3RlIDI6KiogWW91IGNhbiB1c2UgdGhlIHVuaWNvZGUgZXNjYXBlcyBmcm9tIGBcXHV7MDAwMH1gIHRvIGBcXHV7MTBGRkZGfWAgdG9cbnJlcHJlc2VudCBjaGFyYWN0ZXJzIGJ5IHRoZWlyIGNvZGUgcG9pbnQuIFlvdSBjYW4gYWxzbyBpbmNsdWRlIHRoZSB1bmljb2RlXG5jaGFyYWN0ZXJzIGRpcmVjdGx5LiBVc2luZyB0aGUgZXNjYXBlcyBjYW4gYmUgYmV0dGVyIGlmIHlvdSBuZWVkIG9uZSBvZiB0aGVcbm1hbnkgd2hpdGVzcGFjZSBjaGFyYWN0ZXJzIHdpdGggZGlmZmVyZW50IHdpZHRocy5cblxuW3VdOiBodHRwczovL2VuLndpa2lwZWRpYS5vcmcvd2lraS9Vbmljb2RlXG5cbi19XG50eXBlIENoYXJcbiAgICA9IENoYXIgLS0gTk9URTogVGhlIGNvbXBpbGVyIHByb3ZpZGVzIHRoZSByZWFsIGltcGxlbWVudGF0aW9uLlxuXG5cblxuLS0gQ0xBU1NJRklDQVRJT05cblxuXG57LXwgRGV0ZWN0IHVwcGVyIGNhc2UgQVNDSUkgY2hhcmFjdGVycy5cblxuICAgIGlzVXBwZXIgJ0EnID09IFRydWVcblxuICAgIGlzVXBwZXIgJ0InXG4gICAgICAgID09IFRydWVcbiAgICAgICAgLi4uIGlzVXBwZXIgJ1onXG4gICAgICAgID09IFRydWVcblxuICAgIGlzVXBwZXIgJzAnID09IEZhbHNlXG5cbiAgICBpc1VwcGVyICdhJyA9PSBGYWxzZVxuXG4gICAgaXNVcHBlciAnLScgPT0gRmFsc2VcblxuICAgIGlzVXBwZXIgJ86jJyA9PSBGYWxzZVxuXG4tfVxuaXNVcHBlciA6IENoYXIgLT4gQm9vbFxuaXNVcHBlciBjaGFyID1cbiAgICBsZXRcbiAgICAgICAgY29kZSA9XG4gICAgICAgICAgICB0b0NvZGUgY2hhclxuICAgIGluXG4gICAgY29kZSA8PSAweDVBICYmIDB4NDEgPD0gY29kZVxuXG5cbnstfCBEZXRlY3QgbG93ZXIgY2FzZSBBU0NJSSBjaGFyYWN0ZXJzLlxuXG4gICAgaXNMb3dlciAnYScgPT0gVHJ1ZVxuXG4gICAgaXNMb3dlciAnYidcbiAgICAgICAgPT0gVHJ1ZVxuICAgICAgICAuLi4gaXNMb3dlciAneidcbiAgICAgICAgPT0gVHJ1ZVxuXG4gICAgaXNMb3dlciAnMCcgPT0gRmFsc2VcblxuICAgIGlzTG93ZXIgJ0EnID09IEZhbHNlXG5cbiAgICBpc0xvd2VyICctJyA9PSBGYWxzZVxuXG4gICAgaXNMb3dlciAnz4AnID09IEZhbHNlXG5cbi19XG5pc0xvd2VyIDogQ2hhciAtPiBCb29sXG5pc0xvd2VyIGNoYXIgPVxuICAgIGxldFxuICAgICAgICBjb2RlID1cbiAgICAgICAgICAgIHRvQ29kZSBjaGFyXG4gICAgaW5cbiAgICAweDYxIDw9IGNvZGUgJiYgY29kZSA8PSAweDdBXG5cblxuey18IERldGVjdCB1cHBlciBjYXNlIGFuZCBsb3dlciBjYXNlIEFTQ0lJIGNoYXJhY3RlcnMuXG5cbiAgICBpc0FscGhhICdhJyA9PSBUcnVlXG5cbiAgICBpc0FscGhhICdiJyA9PSBUcnVlXG5cbiAgICBpc0FscGhhICdFJyA9PSBUcnVlXG5cbiAgICBpc0FscGhhICdZJyA9PSBUcnVlXG5cbiAgICBpc0FscGhhICcwJyA9PSBGYWxzZVxuXG4gICAgaXNBbHBoYSAnLScgPT0gRmFsc2VcblxuICAgIGlzQWxwaGEgJ8+AJyA9PSBGYWxzZVxuXG4tfVxuaXNBbHBoYSA6IENoYXIgLT4gQm9vbFxuaXNBbHBoYSBjaGFyID1cbiAgICBpc0xvd2VyIGNoYXIgfHwgaXNVcHBlciBjaGFyXG5cblxuey18IERldGVjdCB1cHBlciBjYXNlIGFuZCBsb3dlciBjYXNlIEFTQ0lJIGNoYXJhY3RlcnMuXG5cbiAgICBpc0FscGhhTnVtICdhJyA9PSBUcnVlXG5cbiAgICBpc0FscGhhTnVtICdiJyA9PSBUcnVlXG5cbiAgICBpc0FscGhhTnVtICdFJyA9PSBUcnVlXG5cbiAgICBpc0FscGhhTnVtICdZJyA9PSBUcnVlXG5cbiAgICBpc0FscGhhTnVtICcwJyA9PSBUcnVlXG5cbiAgICBpc0FscGhhTnVtICc3JyA9PSBUcnVlXG5cbiAgICBpc0FscGhhTnVtICctJyA9PSBGYWxzZVxuXG4gICAgaXNBbHBoYU51bSAnz4AnID09IEZhbHNlXG5cbi19XG5pc0FscGhhTnVtIDogQ2hhciAtPiBCb29sXG5pc0FscGhhTnVtIGNoYXIgPVxuICAgIGlzTG93ZXIgY2hhciB8fCBpc1VwcGVyIGNoYXIgfHwgaXNEaWdpdCBjaGFyXG5cblxuey18IERldGVjdCBkaWdpdHMgYDAxMjM0NTY3ODlgXG5cbiAgICBpc0RpZ2l0ICcwJyA9PSBUcnVlXG5cbiAgICBpc0RpZ2l0ICcxJ1xuICAgICAgICA9PSBUcnVlXG4gICAgICAgIC4uLiBpc0RpZ2l0ICc5J1xuICAgICAgICA9PSBUcnVlXG5cbiAgICBpc0RpZ2l0ICdhJyA9PSBGYWxzZVxuXG4gICAgaXNEaWdpdCAnYicgPT0gRmFsc2VcblxuICAgIGlzRGlnaXQgJ0EnID09IEZhbHNlXG5cbi19XG5pc0RpZ2l0IDogQ2hhciAtPiBCb29sXG5pc0RpZ2l0IGNoYXIgPVxuICAgIGxldFxuICAgICAgICBjb2RlID1cbiAgICAgICAgICAgIHRvQ29kZSBjaGFyXG4gICAgaW5cbiAgICBjb2RlIDw9IDB4MzkgJiYgMHgzMCA8PSBjb2RlXG5cblxuey18IERldGVjdCBvY3RhbCBkaWdpdHMgYDAxMjM0NTY3YFxuXG4gICAgaXNPY3REaWdpdCAnMCcgPT0gVHJ1ZVxuXG4gICAgaXNPY3REaWdpdCAnMSdcbiAgICAgICAgPT0gVHJ1ZVxuICAgICAgICAuLi4gaXNPY3REaWdpdCAnNydcbiAgICAgICAgPT0gVHJ1ZVxuXG4gICAgaXNPY3REaWdpdCAnOCcgPT0gRmFsc2VcblxuICAgIGlzT2N0RGlnaXQgJ2EnID09IEZhbHNlXG5cbiAgICBpc09jdERpZ2l0ICdBJyA9PSBGYWxzZVxuXG4tfVxuaXNPY3REaWdpdCA6IENoYXIgLT4gQm9vbFxuaXNPY3REaWdpdCBjaGFyID1cbiAgICBsZXRcbiAgICAgICAgY29kZSA9XG4gICAgICAgICAgICB0b0NvZGUgY2hhclxuICAgIGluXG4gICAgY29kZSA8PSAweDM3ICYmIDB4MzAgPD0gY29kZVxuXG5cbnstfCBEZXRlY3QgaGV4YWRlY2ltYWwgZGlnaXRzIGAwMTIzNDU2Nzg5YWJjZGVmQUJDREVGYFxuLX1cbmlzSGV4RGlnaXQgOiBDaGFyIC0+IEJvb2xcbmlzSGV4RGlnaXQgY2hhciA9XG4gICAgbGV0XG4gICAgICAgIGNvZGUgPVxuICAgICAgICAgICAgdG9Db2RlIGNoYXJcbiAgICBpblxuICAgICgweDMwIDw9IGNvZGUgJiYgY29kZSA8PSAweDM5KVxuICAgICAgICB8fCAoMHg0MSA8PSBjb2RlICYmIGNvZGUgPD0gMHg0NilcbiAgICAgICAgfHwgKDB4NjEgPD0gY29kZSAmJiBjb2RlIDw9IDB4NjYpXG5cblxuXG4tLSBDT05WRVJTSU9OU1xuXG5cbnstfCBDb252ZXJ0IHRvIHVwcGVyIGNhc2UuXG4tfVxudG9VcHBlciA6IENoYXIgLT4gQ2hhclxudG9VcHBlciA9XG4gICAgR3Jlbi5LZXJuZWwuQ2hhci50b1VwcGVyXG5cblxuey18IENvbnZlcnQgdG8gbG93ZXIgY2FzZS5cbi19XG50b0xvd2VyIDogQ2hhciAtPiBDaGFyXG50b0xvd2VyID1cbiAgICBHcmVuLktlcm5lbC5DaGFyLnRvTG93ZXJcblxuXG57LXwgQ29udmVydCB0byB1cHBlciBjYXNlLCBhY2NvcmRpbmcgdG8gYW55IGxvY2FsZS1zcGVjaWZpYyBjYXNlIG1hcHBpbmdzLlxuLX1cbnRvTG9jYWxlVXBwZXIgOiBDaGFyIC0+IENoYXJcbnRvTG9jYWxlVXBwZXIgPVxuICAgIEdyZW4uS2VybmVsLkNoYXIudG9Mb2NhbGVVcHBlclxuXG5cbnstfCBDb252ZXJ0IHRvIGxvd2VyIGNhc2UsIGFjY29yZGluZyB0byBhbnkgbG9jYWxlLXNwZWNpZmljIGNhc2UgbWFwcGluZ3MuXG4tfVxudG9Mb2NhbGVMb3dlciA6IENoYXIgLT4gQ2hhclxudG9Mb2NhbGVMb3dlciA9XG4gICAgR3Jlbi5LZXJuZWwuQ2hhci50b0xvY2FsZUxvd2VyXG5cblxuey18IENvbnZlcnQgdG8gdGhlIGNvcnJlc3BvbmRpbmcgVW5pY29kZSBbY29kZSBwb2ludF1bY3BdLlxuXG5bY3BdOiBodHRwczovL2VuLndpa2lwZWRpYS5vcmcvd2lraS9Db2RlX3BvaW50XG5cbiAgICB0b0NvZGUgJ0EnID09IDY1XG5cbiAgICB0b0NvZGUgJ0InID09IDY2XG5cbiAgICB0b0NvZGUgJ+acqCcgPT0gMHg2NzI4XG5cbiAgICB0b0NvZGUgJ/CdjIYnID09IDB4MDAwMUQzMDZcblxuICAgIHRvQ29kZSAn8J+YgycgPT0gMHgwMDAxRjYwM1xuXG4tfVxudG9Db2RlIDogQ2hhciAtPiBJbnRcbnRvQ29kZSA9XG4gICAgR3Jlbi5LZXJuZWwuQ2hhci50b0NvZGVcblxuXG57LXwgQ29udmVydCBhIFVuaWNvZGUgW2NvZGUgcG9pbnRdW2NwXSB0byBhIGNoYXJhY3Rlci5cblxuICAgIGZyb21Db2RlIDY1ID09ICdBJ1xuXG4gICAgZnJvbUNvZGUgNjYgPT0gJ0InXG5cbiAgICBmcm9tQ29kZSAweDY3MjggPT0gJ+acqCdcblxuICAgIGZyb21Db2RlIDB4MDAwMUQzMDYgPT0gJ/CdjIYnXG5cbiAgICBmcm9tQ29kZSAweDAwMDFGNjAzID09ICfwn5iDJ1xuXG4gICAgZnJvbUNvZGUgLTEgPT0gJ++/vSdcblxuVGhlIGZ1bGwgcmFuZ2Ugb2YgdW5pY29kZSBpcyBmcm9tIGAwYCB0byBgMHgxMEZGRkZgLiBXaXRoIG51bWJlcnMgb3V0c2lkZSB0aGF0XG5yYW5nZSwgeW91IGdldCBbdGhlIHJlcGxhY2VtZW50IGNoYXJhY3Rlcl1bZmZmZF0uXG5cbltjcF06IGh0dHBzOi8vZW4ud2lraXBlZGlhLm9yZy93aWtpL0NvZGVfcG9pbnRcbltmZmZkXTogaHR0cHM6Ly9lbi53aWtpcGVkaWEub3JnL3dpa2kvU3BlY2lhbHNfKFVuaWNvZGVfYmxvY2spI1JlcGxhY2VtZW50X2NoYXJhY3RlclxuXG4tfVxuZnJvbUNvZGUgOiBJbnQgLT4gQ2hhclxuZnJvbUNvZGUgPVxuICAgIEdyZW4uS2VybmVsLkNoYXIuZnJvbUNvZGVcbiIsCiAgICAgICAgIm1vZHVsZSBSZXN1bHQgZXhwb3NpbmdcbiAgICAoIFJlc3VsdCguLilcbiAgICAsIG1hcCwgbWFwMiwgbWFwMywgbWFwNCwgbWFwNVxuICAgICwgYW5kVGhlblxuICAgICwgd2l0aERlZmF1bHQsIHRvTWF5YmUsIGZyb21NYXliZSwgbWFwRXJyb3JcbiAgICApXG5cbnstfCBBIGBSZXN1bHRgIGlzIHRoZSByZXN1bHQgb2YgYSBjb21wdXRhdGlvbiB0aGF0IG1heSBmYWlsLiBUaGlzIGlzIGEgZ3JlYXRcbndheSB0byBtYW5hZ2UgZXJyb3JzIGluIEdyZW4uXG5cblxuQGRvY3MgUmVzdWx0XG5cblxuIyMgTWFwcGluZ1xuXG5AZG9jcyBtYXAsIG1hcDIsIG1hcDMsIG1hcDQsIG1hcDVcblxuXG4jIyBDaGFpbmluZ1xuXG5AZG9jcyBhbmRUaGVuXG5cblxuIyMgSGFuZGxpbmcgRXJyb3JzXG5cbkBkb2NzIHdpdGhEZWZhdWx0LCB0b01heWJlLCBmcm9tTWF5YmUsIG1hcEVycm9yXG5cbi19XG5cbmltcG9ydCBCYXNpY3MgZXhwb3NpbmcgKEJvb2woLi4pKVxuaW1wb3J0IE1heWJlIGV4cG9zaW5nIChNYXliZSguLikpXG5cblxuey18IEEgYFJlc3VsdGAgaXMgZWl0aGVyIGBPa2AgbWVhbmluZyB0aGUgY29tcHV0YXRpb24gc3VjY2VlZGVkLCBvciBpdCBpcyBhblxuYEVycmAgbWVhbmluZyB0aGF0IHRoZXJlIHdhcyBzb21lIGZhaWx1cmUuXG4tfVxudHlwZSBSZXN1bHQgZXJyb3IgdmFsdWVcbiAgICA9IE9rIHZhbHVlXG4gICAgfCBFcnIgZXJyb3JcblxuXG57LXwgSWYgdGhlIHJlc3VsdCBpcyBgT2tgIHJldHVybiB0aGUgdmFsdWUsIGJ1dCBpZiB0aGUgcmVzdWx0IGlzIGFuIGBFcnJgIHRoZW5cbnJldHVybiBhIGdpdmVuIGRlZmF1bHQgdmFsdWUuIFRoZSBmb2xsb3dpbmcgZXhhbXBsZXMgdHJ5IHRvIHBhcnNlIGludGVnZXJzLlxuXG4gICAgUmVzdWx0LndpdGhEZWZhdWx0IDAgKE9rIDEyMykgPT0gMTIzXG5cbiAgICBSZXN1bHQud2l0aERlZmF1bHQgMCAoRXJyIFwibm9cIikgPT0gMFxuXG4tfVxud2l0aERlZmF1bHQgOiBhIC0+IFJlc3VsdCB4IGEgLT4gYVxud2l0aERlZmF1bHQgZGVmIHJlc3VsdCA9XG4gICAgY2FzZSByZXN1bHQgb2ZcbiAgICAgICAgT2sgYSAtPlxuICAgICAgICAgICAgYVxuXG4gICAgICAgIEVyciBfIC0+XG4gICAgICAgICAgICBkZWZcblxuXG57LXwgQXBwbHkgYSBmdW5jdGlvbiB0byBhIHJlc3VsdC4gSWYgdGhlIHJlc3VsdCBpcyBgT2tgLCBpdCB3aWxsIGJlIGNvbnZlcnRlZC5cbklmIHRoZSByZXN1bHQgaXMgYW4gYEVycmAsIHRoZSBzYW1lIGVycm9yIHZhbHVlIHdpbGwgcHJvcGFnYXRlIHRocm91Z2guXG5cbiAgICBtYXAgc3FydCAoT2sgNC4wKSA9PSBPayAyLjBcblxuICAgIG1hcCBzcXJ0IChFcnIgXCJiYWQgaW5wdXRcIikgPT0gRXJyIFwiYmFkIGlucHV0XCJcblxuLX1cbm1hcCA6IChhIC0+IHZhbHVlKSAtPiBSZXN1bHQgeCBhIC0+IFJlc3VsdCB4IHZhbHVlXG5tYXAgZnVuYyByYSA9XG4gICAgY2FzZSByYSBvZlxuICAgICAgICBPayBhIC0+XG4gICAgICAgICAgICBPayAoZnVuYyBhKVxuXG4gICAgICAgIEVyciBlIC0+XG4gICAgICAgICAgICBFcnIgZVxuXG5cbnstfCBBcHBseSBhIGZ1bmN0aW9uIGlmIGJvdGggcmVzdWx0cyBhcmUgYE9rYC4gSWYgbm90LCB0aGUgZmlyc3QgYEVycmAgd2lsbFxucHJvcGFnYXRlIHRocm91Z2guXG5cbiAgICBtYXAyIG1heCAoT2sgNDIpIChPayAxMykgPT0gT2sgNDJcblxuICAgIG1hcDIgbWF4IChFcnIgXCJ4XCIpIChPayAxMykgPT0gRXJyIFwieFwiXG5cbiAgICBtYXAyIG1heCAoT2sgNDIpIChFcnIgXCJ5XCIpID09IEVyciBcInlcIlxuXG4gICAgbWFwMiBtYXggKEVyciBcInhcIikgKEVyciBcInlcIikgPT0gRXJyIFwieFwiXG5cblRoaXMgY2FuIGJlIHVzZWZ1bCBpZiB5b3UgaGF2ZSB0d28gY29tcHV0YXRpb25zIHRoYXQgbWF5IGZhaWwsIGFuZCB5b3Ugd2FudFxudG8gcHV0IHRoZW0gdG9nZXRoZXIgcXVpY2tseS5cblxuLX1cbm1hcDIgOiAoYSAtPiBiIC0+IHZhbHVlKSAtPiBSZXN1bHQgeCBhIC0+IFJlc3VsdCB4IGIgLT4gUmVzdWx0IHggdmFsdWVcbm1hcDIgZnVuYyByYSByYiA9XG4gICAgY2FzZSByYSBvZlxuICAgICAgICBFcnIgeCAtPlxuICAgICAgICAgICAgRXJyIHhcblxuICAgICAgICBPayBhIC0+XG4gICAgICAgICAgICBjYXNlIHJiIG9mXG4gICAgICAgICAgICAgICAgRXJyIHggLT5cbiAgICAgICAgICAgICAgICAgICAgRXJyIHhcblxuICAgICAgICAgICAgICAgIE9rIGIgLT5cbiAgICAgICAgICAgICAgICAgICAgT2sgKGZ1bmMgYSBiKVxuXG5cbnstfCAtfVxubWFwMyA6IChhIC0+IGIgLT4gYyAtPiB2YWx1ZSkgLT4gUmVzdWx0IHggYSAtPiBSZXN1bHQgeCBiIC0+IFJlc3VsdCB4IGMgLT4gUmVzdWx0IHggdmFsdWVcbm1hcDMgZnVuYyByYSByYiByYyA9XG4gICAgY2FzZSByYSBvZlxuICAgICAgICBFcnIgeCAtPlxuICAgICAgICAgICAgRXJyIHhcblxuICAgICAgICBPayBhIC0+XG4gICAgICAgICAgICBjYXNlIHJiIG9mXG4gICAgICAgICAgICAgICAgRXJyIHggLT5cbiAgICAgICAgICAgICAgICAgICAgRXJyIHhcblxuICAgICAgICAgICAgICAgIE9rIGIgLT5cbiAgICAgICAgICAgICAgICAgICAgY2FzZSByYyBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgRXJyIHggLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBFcnIgeFxuXG4gICAgICAgICAgICAgICAgICAgICAgICBPayBjIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgT2sgKGZ1bmMgYSBiIGMpXG5cblxuey18IC19XG5tYXA0IDogKGEgLT4gYiAtPiBjIC0+IGQgLT4gdmFsdWUpIC0+IFJlc3VsdCB4IGEgLT4gUmVzdWx0IHggYiAtPiBSZXN1bHQgeCBjIC0+IFJlc3VsdCB4IGQgLT4gUmVzdWx0IHggdmFsdWVcbm1hcDQgZnVuYyByYSByYiByYyByZCA9XG4gICAgY2FzZSByYSBvZlxuICAgICAgICBFcnIgeCAtPlxuICAgICAgICAgICAgRXJyIHhcblxuICAgICAgICBPayBhIC0+XG4gICAgICAgICAgICBjYXNlIHJiIG9mXG4gICAgICAgICAgICAgICAgRXJyIHggLT5cbiAgICAgICAgICAgICAgICAgICAgRXJyIHhcblxuICAgICAgICAgICAgICAgIE9rIGIgLT5cbiAgICAgICAgICAgICAgICAgICAgY2FzZSByYyBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgRXJyIHggLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBFcnIgeFxuXG4gICAgICAgICAgICAgICAgICAgICAgICBPayBjIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgY2FzZSByZCBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBFcnIgeCAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgRXJyIHhcblxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBPayBkIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBPayAoZnVuYyBhIGIgYyBkKVxuXG5cbnstfCAtfVxubWFwNSA6IChhIC0+IGIgLT4gYyAtPiBkIC0+IGUgLT4gdmFsdWUpIC0+IFJlc3VsdCB4IGEgLT4gUmVzdWx0IHggYiAtPiBSZXN1bHQgeCBjIC0+IFJlc3VsdCB4IGQgLT4gUmVzdWx0IHggZSAtPiBSZXN1bHQgeCB2YWx1ZVxubWFwNSBmdW5jIHJhIHJiIHJjIHJkIHJlID1cbiAgICBjYXNlIHJhIG9mXG4gICAgICAgIEVyciB4IC0+XG4gICAgICAgICAgICBFcnIgeFxuXG4gICAgICAgIE9rIGEgLT5cbiAgICAgICAgICAgIGNhc2UgcmIgb2ZcbiAgICAgICAgICAgICAgICBFcnIgeCAtPlxuICAgICAgICAgICAgICAgICAgICBFcnIgeFxuXG4gICAgICAgICAgICAgICAgT2sgYiAtPlxuICAgICAgICAgICAgICAgICAgICBjYXNlIHJjIG9mXG4gICAgICAgICAgICAgICAgICAgICAgICBFcnIgeCAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEVyciB4XG5cbiAgICAgICAgICAgICAgICAgICAgICAgIE9rIGMgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBjYXNlIHJkIG9mXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEVyciB4IC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBFcnIgeFxuXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIE9rIGQgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGNhc2UgcmUgb2ZcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBFcnIgeCAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBFcnIgeFxuXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgT2sgZSAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBPayAoZnVuYyBhIGIgYyBkIGUpXG5cblxuey18IENoYWluIHRvZ2V0aGVyIGEgc2VxdWVuY2Ugb2YgY29tcHV0YXRpb25zIHRoYXQgbWF5IGZhaWwuIEl0IGlzIGhlbHBmdWxcbnRvIHNlZSBpdHMgZGVmaW5pdGlvbjpcblxuICAgIGFuZFRoZW4gOiAoYSAtPiBSZXN1bHQgZSBiKSAtPiBSZXN1bHQgZSBhIC0+IFJlc3VsdCBlIGJcbiAgICBhbmRUaGVuIGNhbGxiYWNrIHJlc3VsdCA9XG4gICAgICAgIGNhc2UgcmVzdWx0IG9mXG4gICAgICAgICAgICBPayB2YWx1ZSAtPlxuICAgICAgICAgICAgICAgIGNhbGxiYWNrIHZhbHVlXG5cbiAgICAgICAgICAgIEVyciBtc2cgLT5cbiAgICAgICAgICAgICAgICBFcnIgbXNnXG5cblRoaXMgbWVhbnMgd2Ugb25seSBjb250aW51ZSB3aXRoIHRoZSBjYWxsYmFjayBpZiB0aGluZ3MgYXJlIGdvaW5nIHdlbGwuIEZvclxuZXhhbXBsZSwgc2F5IHlvdSBuZWVkIHRvIHVzZSAoYHRvSW50IDogU3RyaW5nIC0+IFJlc3VsdCBTdHJpbmcgSW50YCkgdG8gcGFyc2VcbmEgbW9udGggYW5kIG1ha2Ugc3VyZSBpdCBpcyBiZXR3ZWVuIDEgYW5kIDEyOlxuXG5cbiAgICB0b1ZhbGlkTW9udGggOiBJbnQgLT4gUmVzdWx0IFN0cmluZyBJbnRcbiAgICB0b1ZhbGlkTW9udGggbW9udGggPVxuICAgICAgICBpZiBtb250aCA+PSAxICYmIG1vbnRoIDw9IDEyIHRoZW5cbiAgICAgICAgICAgIE9rIG1vbnRoXG5cbiAgICAgICAgZWxzZVxuICAgICAgICAgICAgRXJyIFwibW9udGhzIG11c3QgYmUgYmV0d2VlbiAxIGFuZCAxMlwiXG5cbiAgICB0b01vbnRoIDogU3RyaW5nIC0+IFJlc3VsdCBTdHJpbmcgSW50XG4gICAgdG9Nb250aCByYXdTdHJpbmcgPVxuICAgICAgICB0b0ludCByYXdTdHJpbmdcbiAgICAgICAgICAgIHw+IGFuZFRoZW4gdG9WYWxpZE1vbnRoXG5cbiAgICAtLSB0b01vbnRoIFwiNFwiID09IE9rIDRcbiAgICAtLSB0b01vbnRoIFwiOVwiID09IE9rIDlcbiAgICAtLSB0b01vbnRoIFwiYVwiID09IEVyciBcImNhbm5vdCBwYXJzZSB0byBhbiBJbnRcIlxuICAgIC0tIHRvTW9udGggXCIwXCIgPT0gRXJyIFwibW9udGhzIG11c3QgYmUgYmV0d2VlbiAxIGFuZCAxMlwiXG5cblRoaXMgYWxsb3dzIHVzIHRvIGNvbWUgb3V0IG9mIGEgY2hhaW4gb2Ygb3BlcmF0aW9ucyB3aXRoIHF1aXRlIGEgc3BlY2lmaWMgZXJyb3Jcbm1lc3NhZ2UuIEl0IGlzIG9mdGVuIGJlc3QgdG8gY3JlYXRlIGEgY3VzdG9tIHR5cGUgdGhhdCBleHBsaWNpdGx5IHJlcHJlc2VudHNcbnRoZSBleGFjdCB3YXlzIHlvdXIgY29tcHV0YXRpb24gbWF5IGZhaWwuIFRoaXMgd2F5IGl0IGlzIGVhc3kgdG8gaGFuZGxlIGluIHlvdXJcbmNvZGUuXG5cbi19XG5hbmRUaGVuIDogKGEgLT4gUmVzdWx0IHggYikgLT4gUmVzdWx0IHggYSAtPiBSZXN1bHQgeCBiXG5hbmRUaGVuIGNhbGxiYWNrIHJlc3VsdCA9XG4gICAgY2FzZSByZXN1bHQgb2ZcbiAgICAgICAgT2sgdmFsdWUgLT5cbiAgICAgICAgICAgIGNhbGxiYWNrIHZhbHVlXG5cbiAgICAgICAgRXJyIG1zZyAtPlxuICAgICAgICAgICAgRXJyIG1zZ1xuXG5cbnstfCBUcmFuc2Zvcm0gYW4gYEVycmAgdmFsdWUuIEZvciBleGFtcGxlLCBzYXkgdGhlIGVycm9ycyB3ZSBnZXQgaGF2ZSB0b28gbXVjaFxuaW5mb3JtYXRpb246XG5cbiAgICBwYXJzZUludCA6IFN0cmluZyAtPiBSZXN1bHQgUGFyc2VFcnJvciBJbnRcblxuICAgIHR5cGUgYWxpYXMgUGFyc2VFcnJvciA9XG4gICAgICAgIHsgbWVzc2FnZSA6IFN0cmluZ1xuICAgICAgICAsIGNvZGUgOiBJbnRcbiAgICAgICAgLCBwb3NpdGlvbiA6IChJbnQsSW50KVxuICAgICAgICB9XG5cbiAgICBtYXBFcnJvciAubWVzc2FnZSAocGFyc2VJbnQgXCIxMjNcIikgPT0gT2sgMTIzXG4gICAgbWFwRXJyb3IgLm1lc3NhZ2UgKHBhcnNlSW50IFwiYWJjXCIpID09IEVyciBcImNoYXIgJ2EnIGlzIG5vdCBhIG51bWJlclwiXG5cbi19XG5tYXBFcnJvciA6ICh4IC0+IHkpIC0+IFJlc3VsdCB4IGEgLT4gUmVzdWx0IHkgYVxubWFwRXJyb3IgZiByZXN1bHQgPVxuICAgIGNhc2UgcmVzdWx0IG9mXG4gICAgICAgIE9rIHYgLT5cbiAgICAgICAgICAgIE9rIHZcblxuICAgICAgICBFcnIgZSAtPlxuICAgICAgICAgICAgRXJyIChmIGUpXG5cblxuey18IENvbnZlcnQgdG8gYSBzaW1wbGVyIGBNYXliZWAgaWYgdGhlIGFjdHVhbCBlcnJvciBtZXNzYWdlIGlzIG5vdCBuZWVkZWQgb3JcbnlvdSBuZWVkIHRvIGludGVyYWN0IHdpdGggc29tZSBjb2RlIHRoYXQgcHJpbWFyaWx5IHVzZXMgbWF5YmVzLlxuXG4gICAgcGFyc2VJbnQgOiBTdHJpbmcgLT4gUmVzdWx0IFBhcnNlRXJyb3IgSW50XG5cbiAgICBtYXliZVBhcnNlSW50IDogU3RyaW5nIC0+IE1heWJlIEludFxuICAgIG1heWJlUGFyc2VJbnQgc3RyaW5nID1cbiAgICAgICAgdG9NYXliZSAocGFyc2VJbnQgc3RyaW5nKVxuXG4tfVxudG9NYXliZSA6IFJlc3VsdCB4IGEgLT4gTWF5YmUgYVxudG9NYXliZSByZXN1bHQgPVxuICAgIGNhc2UgcmVzdWx0IG9mXG4gICAgICAgIE9rIHYgLT5cbiAgICAgICAgICAgIEp1c3QgdlxuXG4gICAgICAgIEVyciBfIC0+XG4gICAgICAgICAgICBOb3RoaW5nXG5cblxuey18IENvbnZlcnQgZnJvbSBhIHNpbXBsZSBgTWF5YmVgIHRvIGludGVyYWN0IHdpdGggc29tZSBjb2RlIHRoYXQgcHJpbWFyaWx5XG51c2VzIGBSZXN1bHRzYC5cblxuICAgIHBhcnNlSW50IDogU3RyaW5nIC0+IE1heWJlIEludFxuXG4gICAgcmVzdWx0UGFyc2VJbnQgOiBTdHJpbmcgLT4gUmVzdWx0IFN0cmluZyBJbnRcbiAgICByZXN1bHRQYXJzZUludCBzdHJpbmcgPVxuICAgICAgICBmcm9tTWF5YmUgKFwiZXJyb3IgcGFyc2luZyBzdHJpbmc6IFwiICsrIHRvU3RyaW5nIHN0cmluZykgKHBhcnNlSW50IHN0cmluZylcblxuLX1cbmZyb21NYXliZSA6IHggLT4gTWF5YmUgYSAtPiBSZXN1bHQgeCBhXG5mcm9tTWF5YmUgZXJyIG1heWJlID1cbiAgICBjYXNlIG1heWJlIG9mXG4gICAgICAgIEp1c3QgdiAtPlxuICAgICAgICAgICAgT2sgdlxuXG4gICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgIEVyciBlcnJcblxuXG5cbi0tIEZPUiBJTlRFUk5BTCBVU0UgT05MWVxuLS1cbi0tIFVzZSBgY2FzZWAgZXhwcmVzc2lvbnMgZm9yIHRoaXMgaW4gR3JlbiBjb2RlIVxuXG5cbmlzT2sgOiBSZXN1bHQgeCBhIC0+IEJvb2xcbmlzT2sgcmVzdWx0ID1cbiAgICBjYXNlIHJlc3VsdCBvZlxuICAgICAgICBPayBfIC0+XG4gICAgICAgICAgICBUcnVlXG5cbiAgICAgICAgRXJyIF8gLT5cbiAgICAgICAgICAgIEZhbHNlXG4iLAogICAgICAgICJtb2R1bGUgTm9kZSBleHBvc2luZ1xuICAgICggRW52aXJvbm1lbnRcbiAgICAsIFBsYXRmb3JtKC4uKVxuICAgICwgQ3B1QXJjaGl0ZWN0dXJlKC4uKVxuICAgICwgZ2V0RW52aXJvbm1lbnRWYXJpYWJsZXNcbiAgICAtLVxuICAgICwgU2ltcGxlUHJvZ3JhbVxuICAgICwgZGVmaW5lU2ltcGxlUHJvZ3JhbVxuICAgICwgZW5kV2l0aENtZFxuICAgIC0tXG4gICAgLCBQcm9ncmFtXG4gICAgLCBQcm9ncmFtQ29uZmlndXJhdGlvblxuICAgICwgZGVmaW5lUHJvZ3JhbVxuICAgICwgc3RhcnRQcm9ncmFtICAgIFxuICAgIC0tXG4gICAgLCBleGl0XG4gICAgLCBleGl0V2l0aENvZGVcbiAgICAsIHNldEV4aXRDb2RlXG4gICAgKVxuXG5cbnstfCBBIE5vZGVKUyBwcm9ncmFtIGlzIGRlZmluZWQgbGlrZSBhIGJyb3dzZXItYmFzZWQgR3JlbiBwcm9ncmFtLCBleGNlcHQgdGhhdFxudGhlcmUgaXMgbW9yZSBmbGV4aWJpbGl0eSByZWdhcmRpbmcgaG93IGl0IGlzIGluaXRpYWxpemVkLlxuXG5Zb3UgY2FuIGluaXRpYWxpemUgYW55IG51bWJlciBvZiBzdWJzeXN0ZW1zLCBsaWtlIGBGaWxlU3lzdGVtYCBvciBgVGVybWluYWxgLCBiZWZvcmVcbmluaXRpYWxpemluZyB5b3VyIG93biBwcm9ncmFtIHdpdGggdGhlIHJlc3VsdHMgb2YgdGhvc2UgaW5pdGlhbGl6YXRpb25zLlxuXG5BcyBwYXJ0IG9mIGluaXRpYWxpemluZyBhIHN1YnN5c3RlbSwgeW91IHVzdWFsbHkgYWxzbyBnZXQgYWNjZXNzIHRvIGEgdmFsdWUgdGhhdCBwZXJtaXRzXG55b3UgdG8gY29udGFjdCBzYWlkIHN1YnN5c3RlbS4gQmUgY2FyZWZ1bCB3aGF0IGNvZGUgeW91IGdpdmUgdGhlc2UgcGVybWlzc2lvbnMgdG8uXG5cbiMjIFByb2dyYW1cblxuQGRvY3MgUHJvZ3JhbSwgUHJvZ3JhbUNvbmZpZ3VyYXRpb24sIGRlZmluZVByb2dyYW0sIHN0YXJ0UHJvZ3JhbVxuXG4jIyBTaW1wbGUgUHJvZ3JhbVxuXG5AZG9jcyBTaW1wbGVQcm9ncmFtLCBkZWZpbmVTaW1wbGVQcm9ncmFtLCBlbmRXaXRoQ21kXG5cbiMjIEVudmlyb25tZW50IGluZm9ybWF0aW9uXG5cbkBkb2NzIEVudmlyb25tZW50LCBQbGF0Zm9ybSwgQ3B1QXJjaGl0ZWN0dXJlLCBnZXRFbnZpcm9ubWVudFZhcmlhYmxlc1xuXG4jIyBFeGl0XG5cbkBkb2NzIGV4aXQsIGV4aXRXaXRoQ29kZSwgc2V0RXhpdENvZGVcbi19XG5cblxuaW1wb3J0IERpY3QgZXhwb3NpbmcgKCBEaWN0IClcbmltcG9ydCBJbml0XG5pbXBvcnQgSW50ZXJuYWwuSW5pdFxuaW1wb3J0IFRhc2sgZXhwb3NpbmcgKCBUYXNrIClcbmltcG9ydCBHcmVuLktlcm5lbC5Ob2RlXG5pbXBvcnQgSW50ZXJuYWwuU3RyZWFtIGFzIElTdHJlYW1cbmltcG9ydCBGaWxlU3lzdGVtLlBhdGggZXhwb3NpbmcgKFBhdGgpXG5cblxuLS0gRU5WSVJPTk1FTlRcblxuXG57LXwgQ29udGFpbnMgaW5mb3JtYXRpb24gYWJvdXQgdGhlIGVudmlyb25tZW50IHlvdXIgYXBwbGljYXRpb24gd2FzIGluaXRpYXRlZCBpbi5cblxuKiBgcGxhdGZvcm1gIGFuZCBgY3B1QXJjaGl0ZWN0dXJlYCB0ZWxscyB5b3Ugc29tZXRoaW5nIGFib3V0IHRoZSBvcGVyYXRpbmcgc3lzdGVtIGFuZCBtYWNoaW5lIHlvdXIgYXBwbGljYXRpb24gaXMgcnVubmluZyBvbi5cbiogYGFyZ3NgIGlzIGFuIGBBcnJheWAgb2YgdGhlIGFyZ3VtZW50cyBwYXNzZWQgdG8geW91ciBhcHBsaWNhdGlvbi5cbiogYHN0ZG91dGAsIGBzdGRlcnJgIGFuZCBgc3RkaW5gIGFyZSBzdHJlYW1zIHlvdSBjYW4gdXNlIHRvIGNvbW11bmljYXRlIHdpdGggdGhlIG91dHNpZGUgd29ybGQuIFRha2UgYSBjbG9zZXIgbG9vayBhdCB0aGUgYFN0cmVhbWAgbW9kdWxlIGZvciBtb3JlIGluZm9ybWF0aW9uLlxuXG4tfVxudHlwZSBhbGlhcyBFbnZpcm9ubWVudCA9XG4gICAgeyBwbGF0Zm9ybSA6IFBsYXRmb3JtXG4gICAgLCBjcHVBcmNoaXRlY3R1cmUgOiBDcHVBcmNoaXRlY3R1cmVcbiAgICAsIGFwcGxpY2F0aW9uUGF0aCA6IFBhdGhcbiAgICAsIGFyZ3MgOiBBcnJheSBTdHJpbmdcbiAgICAsIHN0ZG91dCA6IElTdHJlYW0uU3RyZWFtXG4gICAgLCBzdGRlcnIgOiBJU3RyZWFtLlN0cmVhbVxuICAgICwgc3RkaW4gOiBJU3RyZWFtLlN0cmVhbVxuICAgIH1cblxuXG5pbml0aWFsaXplRW52aXJvbm1lbnQgOiBUYXNrIE5ldmVyIEVudmlyb25tZW50XG5pbml0aWFsaXplRW52aXJvbm1lbnQgPVxuICAgIEdyZW4uS2VybmVsLk5vZGUuaW5pdFxuICAgICAgICB8PiBUYXNrLm1hcFxuICAgICAgICAgICAgICAgIChcXHJhdyAtPlxuICAgICAgICAgICAgICAgICAgICB7IHBsYXRmb3JtID0gcGxhdGZvcm1Gcm9tU3RyaW5nIHJhdy5wbGF0Zm9ybVxuICAgICAgICAgICAgICAgICAgICAsIGNwdUFyY2hpdGVjdHVyZSA9IGFyY2hGcm9tU3RyaW5nIHJhdy5hcmNoXG4gICAgICAgICAgICAgICAgICAgICwgYXBwbGljYXRpb25QYXRoID0gcmF3LmFwcGxpY2F0aW9uUGF0aFxuICAgICAgICAgICAgICAgICAgICAsIGFyZ3MgPSByYXcuYXJnc1xuICAgICAgICAgICAgICAgICAgICAsIHN0ZG91dCA9IElTdHJlYW0uU3RyZWFtIDAgcmF3LnN0ZG91dFxuICAgICAgICAgICAgICAgICAgICAsIHN0ZGVyciA9IElTdHJlYW0uU3RyZWFtIDEgcmF3LnN0ZGVyclxuICAgICAgICAgICAgICAgICAgICAsIHN0ZGluID0gSVN0cmVhbS5TdHJlYW0gMiByYXcuc3RkaW5cbiAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgIClcblxuXG57LXwgVGhlIHBsYXRmb3JtLCBvciBvcGVyYXRpbmcgc3lzdGVtLCB0aGF0IHlvdXIgYXBwbGljYXRpb24gaXMgcnVubmluZyBvbi5cbi19XG50eXBlIFBsYXRmb3JtXG4gICAgPSBXaW4zMlxuICAgIHwgRGFyd2luXG4gICAgfCBMaW51eFxuICAgIHwgRnJlZUJTRFxuICAgIHwgT3BlbkJTRFxuICAgIHwgU3VuT1NcbiAgICB8IEFpeFxuICAgIHwgVW5rbm93blBsYXRmb3JtIFN0cmluZ1xuXG5cbnBsYXRmb3JtRnJvbVN0cmluZyA6IFN0cmluZyAtPiBQbGF0Zm9ybVxucGxhdGZvcm1Gcm9tU3RyaW5nIHBsYXRmb3JtID1cbiAgICBjYXNlIFN0cmluZy50b0xvd2VyIHBsYXRmb3JtIG9mXG4gICAgICAgIFwid2luMzJcIiAtPlxuICAgICAgICAgICAgV2luMzJcblxuICAgICAgICBcImRhcndpblwiIC0+XG4gICAgICAgICAgICBEYXJ3aW5cblxuICAgICAgICBcImxpbnV4XCIgLT5cbiAgICAgICAgICAgIExpbnV4XG5cbiAgICAgICAgXCJmcmVlYnNkXCIgLT5cbiAgICAgICAgICAgIEZyZWVCU0RcblxuICAgICAgICBcIm9wZW5ic2RcIiAtPlxuICAgICAgICAgICAgT3BlbkJTRFxuXG4gICAgICAgIFwic3Vub3NcIiAtPlxuICAgICAgICAgICAgU3VuT1NcblxuICAgICAgICBcImFpeFwiIC0+XG4gICAgICAgICAgICBBaXhcblxuICAgICAgICBfIC0+XG4gICAgICAgICAgICBVbmtub3duUGxhdGZvcm0gcGxhdGZvcm1cblxuXG57LXwgVGhlIENQVSBhcmNoaXRlY3R1cmUgeW91ciBhcHBsaWNhdGlvbiBpcyBydW5uaW5nIG9uLlxuLX1cbnR5cGUgQ3B1QXJjaGl0ZWN0dXJlXG4gICAgPSBBcm1cbiAgICB8IEFybTY0XG4gICAgfCBJQTMyXG4gICAgfCBNaXBzXG4gICAgfCBNaXBzZWxcbiAgICB8IFBQQ1xuICAgIHwgUFBDNjRcbiAgICB8IFMzOTBcbiAgICB8IFMzOTB4XG4gICAgfCBYNjRcbiAgICB8IFVua25vd25BcmNoaXRlY3R1cmUgU3RyaW5nXG5cblxuYXJjaEZyb21TdHJpbmcgOiBTdHJpbmcgLT4gQ3B1QXJjaGl0ZWN0dXJlXG5hcmNoRnJvbVN0cmluZyBhcmNoID1cbiAgICBjYXNlIFN0cmluZy50b0xvd2VyIGFyY2ggb2ZcbiAgICAgICAgXCJhcm1cIiAtPlxuICAgICAgICAgICAgQXJtXG5cbiAgICAgICAgXCJhcm02NFwiIC0+XG4gICAgICAgICAgICBBcm02NFxuXG4gICAgICAgIFwiaWEzMlwiIC0+XG4gICAgICAgICAgICBJQTMyXG5cbiAgICAgICAgXCJtaXBzXCIgLT5cbiAgICAgICAgICAgIE1pcHNcblxuICAgICAgICBcIm1pcHNlbFwiIC0+XG4gICAgICAgICAgICBNaXBzZWxcblxuICAgICAgICBcInBwY1wiIC0+XG4gICAgICAgICAgICBQUENcblxuICAgICAgICBcInBwYzY0XCIgLT5cbiAgICAgICAgICAgIFBQQzY0XG5cbiAgICAgICAgXCJzMzkwXCIgLT5cbiAgICAgICAgICAgIFMzOTBcblxuICAgICAgICBcInMzOTB4XCIgLT5cbiAgICAgICAgICAgIFMzOTB4XG5cbiAgICAgICAgXCJ4NjRcIiAtPlxuICAgICAgICAgICAgWDY0XG5cbiAgICAgICAgXyAtPlxuICAgICAgICAgICAgVW5rbm93bkFyY2hpdGVjdHVyZSBhcmNoXG5cblxuey18IEdldCBhIGBEaWN0YCBvZiBlbnZpcm9ubWVudCB2YXJpYWJsZXMuXG4tfVxuZ2V0RW52aXJvbm1lbnRWYXJpYWJsZXMgOiBUYXNrIHggKERpY3QgU3RyaW5nIFN0cmluZylcbmdldEVudmlyb25tZW50VmFyaWFibGVzID1cbiAgICBHcmVuLktlcm5lbC5Ob2RlLmdldEVudmlyb25tZW50VmFyaWFibGVzXG5cblxuLS0gUFJPR1JBTVNcblxuXG57LXwgQSBwcm9ncmFtIHRoYXQgZXhlY3V0ZXMgYSBzaW5nbGUgdGFzayBhbmQgdGhlbiBleGl0cy5cbi19XG50eXBlIGFsaWFzIFNpbXBsZVByb2dyYW0gYSA9XG4gICAgUHJvZ3JhbSB7fSBhXG5cblxuey18IFRoZSBkZWZpbml0aW9uIG9mIGEgR3JlbiBwcm9ncmFtIHRoYXQgcnVucyBvbiBOb2RlSlMuXG4tfVxudHlwZSBhbGlhcyBQcm9ncmFtIG1vZGVsIG1zZyA9XG4gICAgUGxhdGZvcm0uUHJvZ3JhbSB7fSAoTW9kZWwgbW9kZWwpIChNc2cgbW9kZWwgbXNnKVxuXG5cbi0tIFRPUCBMRVZFTCBQUk9HUkFNXG5cblxudHlwZSBNb2RlbCBtb2RlbFxuICAgID0gVW5pbml0aWFsaXplZFxuICAgIHwgSW5pdGlhbGl6ZWQgbW9kZWxcblxuXG50eXBlIE1zZyBtb2RlbCBtc2dcbiAgICA9IEluaXREb25lIHsgbW9kZWwgOiBtb2RlbCwgY29tbWFuZCA6IENtZCBtc2cgfVxuICAgIHwgTXNnUmVjZWl2ZWQgbXNnXG5cblxuey18IFRoZSByZXF1aXJlZCBmdW5jdGlvbnMgdGhhdCBkZWZpbmUgYSBwcm9ncmFtLlxuLX1cbnR5cGUgYWxpYXMgUHJvZ3JhbUNvbmZpZ3VyYXRpb24gbW9kZWwgbXNnID1cbiAgICB7IGluaXQgOiBFbnZpcm9ubWVudCAtPiBJbml0LlRhc2sgeyBtb2RlbCA6IG1vZGVsLCBjb21tYW5kIDogQ21kIG1zZyB9XG4gICAgLCB1cGRhdGUgOiBtc2cgLT4gbW9kZWwgLT4geyBtb2RlbCA6IG1vZGVsLCBjb21tYW5kIDogQ21kIG1zZyB9XG4gICAgLCBzdWJzY3JpcHRpb25zIDogbW9kZWwgLT4gU3ViIG1zZ1xuICAgIH1cblxuXG57LXwgRGVmaW5lIGEgcHJvZ3JhbSB3aXRoIGFjY2VzcyB0byBsb25nLWxpdmVkIHN0YXRlIGFuZCB0aGUgYWJpbGl0eSB0byByZXNwb25kIHRvXG5tZXNzYWdlcyBhbmQgbGlzdGVuIHRvIHN1YnNjcmlwdGlvbnMuIElmIHlvdSB3YW50IHRvIGRlZmluZSBhIHNpbXBsZSBhbmQgc2hvcnQtbGl2ZWRcbnByb2dyYW0sIGNoYW5jZXMgYXJlIHlvdSdyZSBsb29raW5nIGZvciBbZGVmaW5lU2ltcGxlUHJvZ3JhbV0oI2RlZmluZVNpbXBsZVByb2dyYW0pIGluc3RlYWQuXG4tfVxuZGVmaW5lUHJvZ3JhbSA6IFByb2dyYW1Db25maWd1cmF0aW9uIG1vZGVsIG1zZyAtPiBQcm9ncmFtIG1vZGVsIG1zZ1xuZGVmaW5lUHJvZ3JhbSBjb25maWcgPVxuICAgIFBsYXRmb3JtLndvcmtlclxuICAgICAgICB7IGluaXQgPSBpbml0IGNvbmZpZy5pbml0XG4gICAgICAgICwgdXBkYXRlID0gdXBkYXRlIGNvbmZpZy51cGRhdGVcbiAgICAgICAgLCBzdWJzY3JpcHRpb25zID0gc3Vic2NyaXB0aW9ucyBjb25maWcuc3Vic2NyaXB0aW9uc1xuICAgICAgICB9XG5cblxuey18IERlZmluZSBhIHNpbXBsZSBwcm9ncmFtIHRoYXQgZG9lc24ndCByZXF1aXJlIGxvbmctbGl2ZWQgc3RhdGUgb3IgdGhlIGFiaWxpdHkgdG8gcmVzcG9uZFxudG8gbWVzc2FnZXMgb3Igc3Vic2NyaXB0aW9ucy4gSWRlYWwgZm9yIHNpbXBsZSBhbmQgc2hvcnQtbGl2ZWQgcHJvZ3JhbXMuXG4tfVxuZGVmaW5lU2ltcGxlUHJvZ3JhbSA6IChFbnZpcm9ubWVudCAtPiBJbml0LlRhc2sgKENtZCBhKSkgLT4gU2ltcGxlUHJvZ3JhbSBhXG5kZWZpbmVTaW1wbGVQcm9ncmFtIGluaXRUYXNrID1cbiAgICBQbGF0Zm9ybS53b3JrZXJcbiAgICAgICAgeyBpbml0ID0gXFxfIC0+XG4gICAgICAgICAgICB7IG1vZGVsID0gVW5pbml0aWFsaXplZFxuICAgICAgICAgICAgLCBjb21tYW5kID1cbiAgICAgICAgICAgICAgICBpbml0aWFsaXplRW52aXJvbm1lbnRcbiAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5hbmRUaGVuIChcXGVudiAtPiB1bndyYXAgPHwgaW5pdFRhc2sgZW52KVxuICAgICAgICAgICAgICAgICAgICB8PiBUYXNrLnBlcmZvcm0gXG4gICAgICAgICAgICAgICAgICAgICAgICAoXFxjbWQgLT4gXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgSW5pdERvbmVcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgeyBtb2RlbCA9IHt9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgY29tbWFuZCA9IGNtZFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICB9XG4gICAgICAgICwgdXBkYXRlID0gdXBkYXRlIChcXF8gXyAtPiB7IG1vZGVsID0ge30sIGNvbW1hbmQgPSBDbWQubm9uZSB9KVxuICAgICAgICAsIHN1YnNjcmlwdGlvbnMgPSAoXFxfIC0+IFN1Yi5ub25lKVxuICAgICAgICB9XG5cblxuaW5pdFxuICAgIDogKEVudmlyb25tZW50IC0+IEluaXQuVGFzayB7IG1vZGVsIDogbW9kZWwsIGNvbW1hbmQgOiBDbWQgbXNnIH0pXG4gICAgLT4ge31cbiAgICAtPiB7IG1vZGVsIDogTW9kZWwgbW9kZWwsIGNvbW1hbmQgOiBDbWQgKE1zZyBtb2RlbCBtc2cpIH1cbmluaXQgaW5pdFRhc2sge30gPVxuICAgIHsgbW9kZWwgPSBVbmluaXRpYWxpemVkXG4gICAgLCBjb21tYW5kID0gXG4gICAgICAgIGluaXRpYWxpemVFbnZpcm9ubWVudFxuICAgICAgICAgICAgfD4gVGFzay5hbmRUaGVuIChcXGVudiAtPiB1bndyYXAgPHwgaW5pdFRhc2sgZW52KVxuICAgICAgICAgICAgfD4gVGFzay5wZXJmb3JtIEluaXREb25lXG4gICAgfVxuXG5cbnVud3JhcCA6IEludGVybmFsLkluaXQuVGFzayBhIC0+IFRhc2sgTmV2ZXIgYVxudW53cmFwIChJbnRlcm5hbC5Jbml0LlRhc2sgdGFzaykgPVxuICAgIHRhc2tcblxuXG51cGRhdGVcbiAgICA6IChtc2cgLT4gbW9kZWwgLT4geyBtb2RlbCA6IG1vZGVsLCBjb21tYW5kIDogQ21kIG1zZyB9KVxuICAgIC0+IE1zZyBtb2RlbCBtc2dcbiAgICAtPiBNb2RlbCBtb2RlbFxuICAgIC0+IHsgbW9kZWwgOiBNb2RlbCBtb2RlbCwgY29tbWFuZCA6IENtZCAoTXNnIG1vZGVsIG1zZykgfVxudXBkYXRlIGFwcFVwZGF0ZSBtc2cgbW9kZWwgPVxuICAgIGNhc2UgbW9kZWwgb2ZcbiAgICAgICAgVW5pbml0aWFsaXplZCAtPlxuICAgICAgICAgICAgY2FzZSBtc2cgb2ZcbiAgICAgICAgICAgICAgICBJbml0RG9uZSBpbml0UmVzdWx0IC0+XG4gICAgICAgICAgICAgICAgICAgIHsgbW9kZWwgPSBJbml0aWFsaXplZCBpbml0UmVzdWx0Lm1vZGVsXG4gICAgICAgICAgICAgICAgICAgICwgY29tbWFuZCA9IENtZC5tYXAgTXNnUmVjZWl2ZWQgaW5pdFJlc3VsdC5jb21tYW5kXG4gICAgICAgICAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgICAgIE1zZ1JlY2VpdmVkIF8gLT5cbiAgICAgICAgICAgICAgICAgICAgLS0gSWdub3JlXG4gICAgICAgICAgICAgICAgICAgIHsgbW9kZWwgPSBtb2RlbCwgY29tbWFuZCA9IENtZC5ub25lIH1cblxuICAgICAgICBJbml0aWFsaXplZCBhcHBNb2RlbCAtPlxuICAgICAgICAgICAgY2FzZSBtc2cgb2ZcbiAgICAgICAgICAgICAgICBJbml0RG9uZSBfIC0+XG4gICAgICAgICAgICAgICAgICAgIC0tIElnbm9yZVxuICAgICAgICAgICAgICAgICAgICB7IG1vZGVsID0gbW9kZWwsIGNvbW1hbmQgPSBDbWQubm9uZSB9XG5cbiAgICAgICAgICAgICAgICBNc2dSZWNlaXZlZCBhcHBNc2cgLT5cbiAgICAgICAgICAgICAgICAgICAgbGV0XG4gICAgICAgICAgICAgICAgICAgICAgICB1cGRhdGVSZXN1bHQgPVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGFwcFVwZGF0ZSBhcHBNc2cgYXBwTW9kZWxcbiAgICAgICAgICAgICAgICAgICAgaW5cbiAgICAgICAgICAgICAgICAgICAgeyBtb2RlbCA9IEluaXRpYWxpemVkIHVwZGF0ZVJlc3VsdC5tb2RlbFxuICAgICAgICAgICAgICAgICAgICAsIGNvbW1hbmQgPSBDbWQubWFwIE1zZ1JlY2VpdmVkIHVwZGF0ZVJlc3VsdC5jb21tYW5kXG4gICAgICAgICAgICAgICAgICAgIH1cblxuXG5zdWJzY3JpcHRpb25zXG4gICAgOiAobW9kZWwgLT4gU3ViIG1zZylcbiAgICAtPiAoTW9kZWwgbW9kZWwpXG4gICAgLT4gU3ViIChNc2cgbW9kZWwgbXNnKVxuc3Vic2NyaXB0aW9ucyBhcHBTdWJzIG1vZGVsID1cbiAgICBjYXNlIG1vZGVsIG9mXG4gICAgICAgIFVuaW5pdGlhbGl6ZWQgLT5cbiAgICAgICAgICAgIFN1Yi5ub25lXG5cbiAgICAgICAgSW5pdGlhbGl6ZWQgYXBwTW9kZWwgLT5cbiAgICAgICAgICAgIFN1Yi5tYXAgTXNnUmVjZWl2ZWQgKGFwcFN1YnMgYXBwTW9kZWwpXG5cblxuey18IFRoaXMgbGV0cyB0aGUgcnVudGltZSBrbm93IHRoYXQgeW91J3JlIGRvbmUgaW5pdGlhbGl6aW5nIG90aGVyIHN1YnN5c3RlbXMsXG5hbmQgdGhhdCB5b3VyIHByb2dyYW0gaXMgcmVhZHkgdG8gc3RhcnQuXG4tfVxuc3RhcnRQcm9ncmFtIDogeyBtb2RlbCA6IG1vZGVsLCBjb21tYW5kIDogQ21kIGNtZCB9IC0+IEluaXQuVGFzayB7IG1vZGVsIDogbW9kZWwsIGNvbW1hbmQgOiBDbWQgY21kIH1cbnN0YXJ0UHJvZ3JhbSBpbml0UmVzdWx0ID1cbiAgICBJbnRlcm5hbC5Jbml0LlRhc2sgKFRhc2suc3VjY2VlZCBpbml0UmVzdWx0KVxuXG5cbnstfCBXaGVuIGRlZmluaW5nIGEgcHJvZ3JhbSB3aXRoIFtkZWZpbmVTaW1wbGVQcm9ncmFtXSgjZGVmaW5lU2ltcGxlUHJvZ3JhbSksIHVzZSB0aGlzIGZ1bmN0aW9uIHRvIGRlZmluZSB0aGVcbmZpbmFsIGNvbW1hbmQgdG8gZXhlY3V0ZS5cbi19XG5lbmRXaXRoQ21kIDogQ21kIGEgLT4gSW5pdC5UYXNrIChDbWQgYSlcbmVuZFdpdGhDbWQgY21kID1cbiAgICBJbnRlcm5hbC5Jbml0LlRhc2sgKFRhc2suc3VjY2VlZCBjbWQpXG5cblxuLS0gRVhJVFxuXG5cbnstfCBUZXJtaW5hdGUgdGhlIHByb2dyYW0gaW1tZWRpYXRseS4gSXQgd2lsbCBub3Qgd2FpdCBmb3IgdGFza3MgbGlrZSBodHRwIGNhbGxzXG5vciBmaWxlIHN5c3RlbSB3cml0ZXMgdG8gY29tcGxldGUuXG5cblRoaXMgZnVuY3Rpb24gaXMgZXF1aXZhbGVudCB0bzpcblxuICAgIGV4aXRXaXRoQ29kZSAwXG4tfVxuZXhpdCA6IFRhc2sgeCB7fVxuZXhpdCA9XG4gICAgZXhpdFdpdGhDb2RlIDBcblxuXG57LXwgVGVybWluYXRlIHRoZSBwcm9ncmFtIGltbWVkaWF0bHkuIEl0IHdpbGwgbm90IHdhaXQgZm9yIHRhc2tzIGxpa2UgaHR0cCBjYWxsc1xub3IgZmlsZSBzeXN0ZW0gd3JpdGVzLCBzbyBvbmx5IHVzZSB0aGlzIGlmIHlvdSd2ZSByZWFjaGVkIGEgc3RhdGUgd2hlcmUgaXQgbWFrZXNcbm5vIHNlbnNlIHRvIGNvbnRpbnVlLlxuXG5UaGUgZXhpdCBjb2RlIGNhbiBiZSByZWFkIGJ5IG90aGVyIHByb2Nlc3NlcyBvbiB5b3VyIHN5c3RlbS4gQW55IHZhbHVlIG90aGVyIHRoYW5cbjAgaXMgY29uc2lkZXJlZCBhbiBlcnJvciwgYnV0IHRoZXJlIGFyZSBubyBvdGhlciBmb3JtYWwgcmVxdWlyZW1lbnRzIGZvciB3aGF0XG5tYWtlcyBhbiBleGl0IGNvZGUuIElmIGFsbCB5b3Ugd2FudCBpcyB0byBzaWduYWxpemUgdGhhdCB5b3VyIGFwcGxpY2F0aW9uIGV4aXRlZFxuZHVlIHRvIGFuIGVycm9yLCAtMSBpcyBhIGdvb2Qgb3B0aW9uLlxuLX1cbmV4aXRXaXRoQ29kZSA6IEludCAtPiBUYXNrIHgge31cbmV4aXRXaXRoQ29kZSBjb2RlID1cbiAgICBHcmVuLktlcm5lbC5Ob2RlLmV4aXRXaXRoQ29kZSBjb2RlXG5cblxuey18IFNldCB0aGUgZXJyb3IgY29kZSB0aGF0IHRoZSBwcm9ncmFtIHdpbGwgcmV0dXJuIG9uY2UgaXQgZmluaXNoZXMuXG5cbk5vdGU6IFRoaXMgd2lsbCBub3QgdGVybWluYXRlIHlvdXIgcHJvZ3JhbSwgc28gdGhpbmdzIGxpa2UgaHR0cCBjYWxsc1xub3Igd3JpdGVzIHRvIHRoZSBmaWxlc3lzdGVtIHdpbGwgYmUgYWxsb3dlZCB0byBjb21wbGV0ZS4gSG93ZXZlcixcbnRoZSBwcm9ncmFtIHdpbGwgb25seSBleGl0IG9uY2UgdGhlcmUgYXJlIG5vIG9uZ29pbmcgdGFza3MuXG4tfVxuc2V0RXhpdENvZGUgOiBJbnQgLT4gVGFzayB4IHt9XG5zZXRFeGl0Q29kZSBjb2RlID1cbiAgICBHcmVuLktlcm5lbC5Ob2RlLnNldEV4aXRDb2RlIGNvZGVcbiIsCiAgICAgICAgIm1vZHVsZSBQbGF0Zm9ybSBleHBvc2luZ1xuICAgICggUHJvZ3JhbSwgd29ya2VyXG4gICAgLCBUYXNrLCBQcm9jZXNzSWRcbiAgICAsIFJvdXRlciwgc2VuZFRvQXBwLCBzZW5kVG9TZWxmXG4gICAgKVxuXG57LXwgVGhpcyBtb2R1bGUgY29udGFpbnMgZGVmaW5pdGlvbnMgaW1wb3J0YW50IHRvIHRoZSBsYW5ndWFnZSBydW50aW1lLlxuWW91J3JlIHVubGlrZWx5IHRvIG1ha2UgZGlyZWN0IHVzZSBvZiB0aGVzZSB0aGluZ3MgeW91cnNlbGYuXG5cblxuQGRvY3MgUHJvZ3JhbSwgd29ya2VyXG5cblxuIyMgVGFza3MgYW5kIFByb2Nlc3Nlc1xuXG5AZG9jcyBUYXNrLCBQcm9jZXNzSWRcblxuXG4jIyBFZmZlY3QgTWFuYWdlciBIZWxwZXJzXG5cbkVmZmVjdCBtYW5hZ2VycyBjYW4gYmUgdmlld2VkIGFzIHByb2dyYW1zLXdpdGhpbi1hLXByb2dyYW0uIFRoZXkgaGF2ZSB0aGVpciBvd25cbnN0YXRlLCBhbmQgY29tbXVuaWNhdGUgd2l0aCB0aGUgYXBwbGljYXRpb24gdXNpbmcgbWVzc2FnZXMuXG5cbkVmZmVjdCBtYW5hZ2VycyBhcmUgdXNlZCBpbnRlcm5hbGx5IGZvciBtYW55IHRoaW5ncywgYnV0IGlzbid0IGNvbnNpZGVyZWQgdG8gYmVcbnRydWx5IHN0YWJsZS4gSXQncyBsaWtlbHkgdGhhdCB0aGlzIGZlYXR1cmUgd2lsbCBiZSByZWRlc2lnbmVkIGluIGEgZnV0dXJlIHJlbGFzZS5cblxuXG5AZG9jcyBSb3V0ZXIsIHNlbmRUb0FwcCwgc2VuZFRvU2VsZlxuXG4tfVxuXG5pbXBvcnQgQmFzaWNzIGV4cG9zaW5nIChOZXZlcilcbmltcG9ydCBHcmVuLktlcm5lbC5QbGF0Zm9ybVxuaW1wb3J0IEdyZW4uS2VybmVsLlNjaGVkdWxlclxuaW1wb3J0IFBsYXRmb3JtLkNtZCBleHBvc2luZyAoQ21kKVxuaW1wb3J0IFBsYXRmb3JtLlN1YiBleHBvc2luZyAoU3ViKVxuXG5cblxuLS0gUFJPR1JBTVNcblxuXG57LXwgQSBgUHJvZ3JhbWAgZGVzY3JpYmVzIGFuIEdyZW4gcHJvZ3JhbSEgSG93IGRvZXMgaXQgcmVhY3QgdG8gaW5wdXQ/IERvZXMgaXRcbnNob3cgYW55dGhpbmcgb24gc2NyZWVuPyBFdGMuXG4tfVxudHlwZSBQcm9ncmFtIGZsYWdzIG1vZGVsIG1zZ1xuICAgID0gUHJvZ3JhbVxuXG5cbnstfCBDcmVhdGUgYSBbaGVhZGxlc3NdIHByb2dyYW0gd2l0aCBubyB1c2VyIGludGVyZmFjZS5cblxuVGhpcyBpcyBncmVhdCBpZiB5b3Ugd2FudCB0byB1c2UgR3JlbiBhcyB0aGUgJmxkcXVvO2JyYWluJnJkcXVvOyBmb3Igc29tZXRoaW5nXG5lbHNlLiBGb3IgZXhhbXBsZSwgeW91IGNvdWxkIHNlbmQgbWVzc2FnZXMgb3V0IHBvcnRzIHRvIG1vZGlmeSB0aGUgRE9NLCBidXQgZG9cbmFsbCB0aGUgY29tcGxleCBsb2dpYyBpbiBHcmVuLlxuXG5baGVhZGxlc3NdOiBodHRwczovL2VuLndpa2lwZWRpYS5vcmcvd2lraS9IZWFkbGVzc19zb2Z0d2FyZVxuXG5Jbml0aWFsaXppbmcgYSBoZWFkbGVzcyBwcm9ncmFtIGZyb20gSmF2YVNjcmlwdCBsb29rcyBsaWtlIHRoaXM6XG5cbmBgYGphdmFzY3JpcHRcbnZhciBhcHAgPSBHcmVuLk15VGhpbmcuaW5pdCgpO1xuYGBgXG5cbklmIHlvdSBfZG9fIHdhbnQgdG8gY29udHJvbCB0aGUgdXNlciBpbnRlcmZhY2UgaW4gR3JlbiwgdGhlIFtgQnJvd3NlcmBdW2Jyb3dzZXJdXG5tb2R1bGUgaGFzIGEgZmV3IHdheXMgdG8gY3JlYXRlIHRoYXQga2luZCBvZiBgUHJvZ3JhbWAgaW5zdGVhZCFcblxuW2hlYWRsZXNzXTogaHR0cHM6Ly9lbi53aWtpcGVkaWEub3JnL3dpa2kvSGVhZGxlc3Nfc29mdHdhcmVcblticm93c2VyXTogL3BhY2thZ2UvZ3Jlbi1sYW5nL2Jyb3dzZXIvbGF0ZXN0L21vZHVsZS9Ccm93c2VyXG5cbi19XG53b3JrZXIgOlxuICAgIHsgaW5pdCA6IGZsYWdzIC0+IHsgbW9kZWwgOiBtb2RlbCwgY29tbWFuZCA6IENtZCBtc2cgfVxuICAgICwgdXBkYXRlIDogbXNnIC0+IG1vZGVsIC0+IHsgbW9kZWwgOiBtb2RlbCwgY29tbWFuZCA6IENtZCBtc2cgfVxuICAgICwgc3Vic2NyaXB0aW9ucyA6IG1vZGVsIC0+IFN1YiBtc2dcbiAgICB9XG4gICAgLT4gUHJvZ3JhbSBmbGFncyBtb2RlbCBtc2dcbndvcmtlciA9XG4gICAgR3Jlbi5LZXJuZWwuUGxhdGZvcm0ud29ya2VyXG5cblxuXG4tLSBUQVNLUyBhbmQgUFJPQ0VTU0VTXG5cblxuey18IEhlYWQgb3ZlciB0byB0aGUgZG9jdW1lbnRhdGlvbiBmb3IgdGhlIFtgVGFza2BdKFRhc2spIG1vZHVsZSBmb3IgbW9yZVxuaW5mb3JtYXRpb24gb24gdGhpcy4gSXQgaXMgb25seSBkZWZpbmVkIGhlcmUgYmVjYXVzZSBpdCBpcyBhIHBsYXRmb3JtXG5wcmltaXRpdmUuXG4tfVxudHlwZSBUYXNrIGVyciBva1xuICAgID0gVGFza1xuXG5cbnstfCBIZWFkIG92ZXIgdG8gdGhlIGRvY3VtZW50YXRpb24gZm9yIHRoZSBbYFByb2Nlc3NgXShQcm9jZXNzKSBtb2R1bGUgZm9yXG5pbmZvcm1hdGlvbiBvbiB0aGlzLiBJdCBpcyBvbmx5IGRlZmluZWQgaGVyZSBiZWNhdXNlIGl0IGlzIGEgcGxhdGZvcm1cbnByaW1pdGl2ZS5cbi19XG50eXBlIFByb2Nlc3NJZFxuICAgID0gUHJvY2Vzc0lkXG5cblxuXG4tLSBFRkZFQ1QgTUFOQUdFUiBJTlRFUk5BTFNcblxuXG57LXwgQW4gZWZmZWN0IG1hbmFnZXIgaGFzIGFjY2VzcyB0byBhIOKAnHJvdXRlcuKAnSB0aGF0IHJvdXRlcyBtZXNzYWdlcyBiZXR3ZWVuXG50aGUgbWFpbiBhcHAgYW5kIHlvdXIgaW5kaXZpZHVhbCBlZmZlY3QgbWFuYWdlci5cbi19XG50eXBlIFJvdXRlciBhcHBNc2cgc2VsZk1zZ1xuICAgID0gUm91dGVyXG5cblxuey18IFNlbmQgdGhlIHJvdXRlciBhIG1lc3NhZ2UgZm9yIHRoZSBtYWluIGxvb3Agb2YgeW91ciBhcHAuIFRoaXMgbWVzc2FnZSB3aWxsXG5iZSBoYW5kbGVkIGJ5IHRoZSBvdmVyYWxsIGB1cGRhdGVgIGZ1bmN0aW9uLCBqdXN0IGxpa2UgZXZlbnRzIGZyb20gYEh0bWxgLlxuLX1cbnNlbmRUb0FwcCA6IFJvdXRlciBtc2cgYSAtPiBtc2cgLT4gVGFzayB4IHt9XG5zZW5kVG9BcHAgPVxuICAgIEdyZW4uS2VybmVsLlBsYXRmb3JtLnNlbmRUb0FwcFxuXG5cbnstfCBTZW5kIHRoZSByb3V0ZXIgYSBtZXNzYWdlIGZvciB5b3VyIGVmZmVjdCBtYW5hZ2VyLiBUaGlzIG1lc3NhZ2Ugd2lsbFxuYmUgcm91dGVkIHRvIHRoZSBgb25TZWxmTXNnYCBmdW5jdGlvbiwgd2hlcmUgeW91IGNhbiB1cGRhdGUgdGhlIHN0YXRlIG9mIHlvdXJcbmVmZmVjdCBtYW5hZ2VyIGFzIG5lY2Vzc2FyeS5cblxuQXMgYW4gZXhhbXBsZSwgdGhlIGVmZmVjdCBtYW5hZ2VyIGZvciB3ZWIgc29ja2V0c1xuXG4tfVxuc2VuZFRvU2VsZiA6IFJvdXRlciBhIG1zZyAtPiBtc2cgLT4gVGFzayB4IHt9XG5zZW5kVG9TZWxmID1cbiAgICBHcmVuLktlcm5lbC5QbGF0Zm9ybS5zZW5kVG9TZWxmXG4iLAogICAgICAgICJtb2R1bGUgUGxhdGZvcm0uU3ViIGV4cG9zaW5nXG4gICAgKCBTdWIsIG5vbmUsIGJhdGNoXG4gICAgLCBtYXBcbiAgICApXG5cbnstfFxuXG4+ICoqTm90ZToqKiBHcmVuIGhhcyAqKm1hbmFnZWQgZWZmZWN0cyoqLCBtZWFuaW5nIHRoYXQgdGhpbmdzIGxpa2UgSFRUUFxuPiByZXF1ZXN0cyBvciB3cml0aW5nIHRvIGRpc2sgYXJlIGFsbCB0cmVhdGVkIGFzIF9kYXRhXyBpbiBHcmVuLiBXaGVuIHRoaXNcbj4gZGF0YSBpcyBnaXZlbiB0byB0aGUgR3JlbiBydW50aW1lIHN5c3RlbSwgaXQgY2FuIGRvIHNvbWUg4oCccXVlcnkgb3B0aW1pemF0aW9u4oCdXG4+IGJlZm9yZSBhY3R1YWxseSBwZXJmb3JtaW5nIHRoZSBlZmZlY3QuIFBlcmhhcHMgdW5leHBlY3RlZGx5LCB0aGlzIG1hbmFnZWRcbj4gZWZmZWN0cyBpZGVhIGlzIHRoZSBoZWFydCBvZiB3aHkgR3JlbiBpcyBzbyBuaWNlIGZvciB0ZXN0aW5nLCByZXVzZSxcbj4gcmVwcm9kdWNpYmlsaXR5LCBldGMuXG4+XG4+IEdyZW4gaGFzIHR3byBraW5kcyBvZiBtYW5hZ2VkIGVmZmVjdHM6IGNvbW1hbmRzIGFuZCBzdWJzY3JpcHRpb25zLlxuXG5cbiMjIFN1YnNjcmlwdGlvbnNcblxuQGRvY3MgU3ViLCBub25lLCBiYXRjaFxuXG5cbiMjIEZhbmN5IFN0dWZmXG5cbkBkb2NzIG1hcFxuXG4tfVxuXG5pbXBvcnQgQXJyYXkgZXhwb3NpbmcgKEFycmF5KVxuaW1wb3J0IEdyZW4uS2VybmVsLlBsYXRmb3JtXG5cblxuXG4tLSBTVUJTQ1JJUFRJT05TXG5cblxuey18IEEgc3Vic2NyaXB0aW9uIGlzIGEgd2F5IG9mIHRlbGxpbmcgR3Jlbiwg4oCcSGV5LCBsZXQgbWUga25vdyBpZiBhbnl0aGluZ1xuaW50ZXJlc3RpbmcgaGFwcGVucyBvdmVyIHRoZXJlIeKAnSBTbyBpZiB5b3Ugd2FudCB0byBsaXN0ZW4gZm9yIG1lc3NhZ2VzIG9uIGEgd2ViXG5zb2NrZXQsIHlvdSB3b3VsZCB0ZWxsIEdyZW4gdG8gY3JlYXRlIGEgc3Vic2NyaXB0aW9uLiBJZiB5b3Ugd2FudCB0byBnZXQgY2xvY2tcbnRpY2tzLCB5b3Ugd291bGQgdGVsbCBHcmVuIHRvIHN1YnNjcmliZSB0byB0aGF0LiBUaGUgY29vbCB0aGluZyBoZXJlIGlzIHRoYXRcbnRoaXMgbWVhbnMgX0dyZW5fIG1hbmFnZXMgYWxsIHRoZSBkZXRhaWxzIG9mIHN1YnNjcmlwdGlvbnMgaW5zdGVhZCBvZiBfeW91Xy5cblNvIGlmIGEgd2ViIHNvY2tldCBnb2VzIGRvd24sIF95b3VfIGRvIG5vdCBuZWVkIHRvIG1hbnVhbGx5IHJlY29ubmVjdCB3aXRoIGFuXG5leHBvbmVudGlhbCBiYWNrb2ZmIHN0cmF0ZWd5LCBfR3Jlbl8gZG9lcyB0aGlzIGFsbCBmb3IgeW91IGJlaGluZCB0aGUgc2NlbmVzIVxuXG5FdmVyeSBgU3ViYCBzcGVjaWZpZXMgKDEpIHdoaWNoIGVmZmVjdHMgeW91IG5lZWQgYWNjZXNzIHRvIGFuZCAoMikgdGhlIHR5cGUgb2Zcbm1lc3NhZ2VzIHRoYXQgd2lsbCBjb21lIGJhY2sgaW50byB5b3VyIGFwcGxpY2F0aW9uLlxuXG4qKk5vdGU6KiogRG8gbm90IHdvcnJ5IGlmIHRoaXMgc2VlbXMgY29uZnVzaW5nIGF0IGZpcnN0ISBBcyB3aXRoIGV2ZXJ5IEdyZW4gdXNlclxuZXZlciwgc3Vic2NyaXB0aW9ucyB3aWxsIG1ha2UgbW9yZSBzZW5zZSBhcyB5b3Ugd29yayB0aHJvdWdoIFt0aGUgR3JlbiBBcmNoaXRlY3R1cmVcblR1dG9yaWFsXShodHRwczovL2d1aWRlLmdyZW4tbGFuZy5vcmcvYXJjaGl0ZWN0dXJlLykgYW5kIHNlZSBob3cgdGhleSBmaXRcbmludG8gYSByZWFsIGFwcGxpY2F0aW9uIVxuXG4tfVxudHlwZSBTdWIgbXNnXG4gICAgPSBTdWJcblxuXG57LXwgVGVsbCB0aGUgcnVudGltZSB0aGF0IHRoZXJlIGFyZSBubyBzdWJzY3JpcHRpb25zLlxuLX1cbm5vbmUgOiBTdWIgbXNnXG5ub25lID1cbiAgICBiYXRjaCBbXVxuXG5cbnstfCBXaGVuIHlvdSBuZWVkIHRvIHN1YnNjcmliZSB0byBtdWx0aXBsZSB0aGluZ3MsIHlvdSBjYW4gY3JlYXRlIGEgYGJhdGNoYCBvZlxuc3Vic2NyaXB0aW9ucy5cblxuKipOb3RlOioqIGBTdWIubm9uZWAgYW5kIGBTdWIuYmF0Y2ggWyBTdWIubm9uZSwgU3ViLm5vbmUgXWAgYW5kXG5gU3ViLmJhdGNoIFtdYCBhbGwgZG8gdGhlIHNhbWUgdGhpbmcuXG5cbi19XG5iYXRjaCA6IEFycmF5IChTdWIgbXNnKSAtPiBTdWIgbXNnXG5iYXRjaCA9XG4gICAgR3Jlbi5LZXJuZWwuUGxhdGZvcm0uYmF0Y2hcblxuXG5cbi0tIEZBTkNZIFNUVUZGXG5cblxuey18IFRyYW5zZm9ybSB0aGUgbWVzc2FnZXMgcHJvZHVjZWQgYnkgYSBzdWJzY3JpcHRpb24uXG5WZXJ5IHNpbWlsYXIgdG8gW2BIdG1sLm1hcGBdKC9wYWNrYWdlL2dyZW4tbGFuZy9icm93c2VyL2xhdGVzdC9tb2R1bGUvSHRtbCNtYXApLlxuXG5UaGlzIGlzIHZlcnkgcmFyZWx5IHVzZWZ1bCBpbiB3ZWxsLXN0cnVjdHVyZWQgR3JlbiBjb2RlLCBzbyBkZWZpbml0ZWx5IHJlYWQgdGhlXG5zZWN0aW9uIG9uIFtzdHJ1Y3R1cmVdIGluIHRoZSBndWlkZSBiZWZvcmUgcmVhY2hpbmcgZm9yIHRoaXMhXG5cbltzdHJ1Y3R1cmVdOiBodHRwczovL2d1aWRlLmdyZW4tbGFuZy5vcmcvd2ViYXBwcy9zdHJ1Y3R1cmUuaHRtbFxuXG4tfVxubWFwIDogKGEgLT4gbXNnKSAtPiBTdWIgYSAtPiBTdWIgbXNnXG5tYXAgPVxuICAgIEdyZW4uS2VybmVsLlBsYXRmb3JtLm1hcFxuIiwKICAgICAgICAibW9kdWxlIFBsYXRmb3JtLkNtZCBleHBvc2luZ1xuICAgICggQ21kLCBub25lLCBiYXRjaFxuICAgICwgbWFwXG4gICAgKVxuXG57LXxcblxuPiAqKk5vdGU6KiogR3JlbiBoYXMgKiptYW5hZ2VkIGVmZmVjdHMqKiwgbWVhbmluZyB0aGF0IHRoaW5ncyBsaWtlIEhUVFBcbj4gcmVxdWVzdHMgb3Igd3JpdGluZyB0byBkaXNrIGFyZSBhbGwgdHJlYXRlZCBhcyBfZGF0YV8gaW4gR3Jlbi4gV2hlbiB0aGlzXG4+IGRhdGEgaXMgZ2l2ZW4gdG8gdGhlIEdyZW4gcnVudGltZSBzeXN0ZW0sIGl0IGNhbiBkbyBzb21lIOKAnHF1ZXJ5IG9wdGltaXphdGlvbuKAnVxuPiBiZWZvcmUgYWN0dWFsbHkgcGVyZm9ybWluZyB0aGUgZWZmZWN0LiBQZXJoYXBzIHVuZXhwZWN0ZWRseSwgdGhpcyBtYW5hZ2VkXG4+IGVmZmVjdHMgaWRlYSBpcyB0aGUgaGVhcnQgb2Ygd2h5IEdyZW4gaXMgc28gbmljZSBmb3IgdGVzdGluZywgcmV1c2UsXG4+IHJlcHJvZHVjaWJpbGl0eSwgZXRjLlxuPlxuPiBHcmVuIGhhcyB0d28ga2luZHMgb2YgbWFuYWdlZCBlZmZlY3RzOiBjb21tYW5kcyBhbmQgc3Vic2NyaXB0aW9ucy5cblxuXG4jIyBDb21tYW5kc1xuXG5AZG9jcyBDbWQsIG5vbmUsIGJhdGNoXG5cblxuIyMgRmFuY3kgU3R1ZmZcblxuQGRvY3MgbWFwXG5cbi19XG5cbmltcG9ydCBBcnJheSBleHBvc2luZyAoQXJyYXkpXG5pbXBvcnQgR3Jlbi5LZXJuZWwuUGxhdGZvcm1cblxuXG5cbi0tIENPTU1BTkRTXG5cblxuey18IEEgY29tbWFuZCBpcyBhIHdheSBvZiB0ZWxsaW5nIEdyZW4sIOKAnEhleSwgSSB3YW50IHlvdSB0byBkbyB0aGlzIHRoaW5nIeKAnVxuU28gaWYgeW91IHdhbnQgdG8gc2VuZCBhbiBIVFRQIHJlcXVlc3QsIHlvdSB3b3VsZCBuZWVkIHRvIGNvbW1hbmQgR3JlbiB0byBkbyBpdC5cbk9yIGlmIHlvdSB3YW50ZWQgdG8gYXNrIGZvciBnZW9sb2NhdGlvbiwgeW91IHdvdWxkIG5lZWQgdG8gY29tbWFuZCBHcmVuIHRvIGdvXG5nZXQgaXQuXG5cbkV2ZXJ5IGBDbWRgIHNwZWNpZmllcyAoMSkgd2hpY2ggZWZmZWN0cyB5b3UgbmVlZCBhY2Nlc3MgdG8gYW5kICgyKSB0aGUgdHlwZSBvZlxubWVzc2FnZXMgdGhhdCB3aWxsIGNvbWUgYmFjayBpbnRvIHlvdXIgYXBwbGljYXRpb24uXG5cbioqTm90ZToqKiBEbyBub3Qgd29ycnkgaWYgdGhpcyBzZWVtcyBjb25mdXNpbmcgYXQgZmlyc3QhIEFzIHdpdGggZXZlcnkgR3JlbiB1c2VyXG5ldmVyLCBjb21tYW5kcyB3aWxsIG1ha2UgbW9yZSBzZW5zZSBhcyB5b3Ugd29yayB0aHJvdWdoIFt0aGUgR3JlbiBBcmNoaXRlY3R1cmVcblR1dG9yaWFsXShodHRwczovL2d1aWRlLmdyZW4tbGFuZy5vcmcvYXJjaGl0ZWN0dXJlLykgYW5kIHNlZSBob3cgdGhleVxuZml0IGludG8gYSByZWFsIGFwcGxpY2F0aW9uIVxuXG4tfVxudHlwZSBDbWQgbXNnXG4gICAgPSBDbWRcblxuXG57LXwgVGVsbCB0aGUgcnVudGltZSB0aGF0IHRoZXJlIGFyZSBubyBjb21tYW5kcy5cbi19XG5ub25lIDogQ21kIG1zZ1xubm9uZSA9XG4gICAgYmF0Y2ggW11cblxuXG57LXwgV2hlbiB5b3UgbmVlZCB0aGUgcnVudGltZSBzeXN0ZW0gdG8gcGVyZm9ybSBhIGNvdXBsZSBjb21tYW5kcywgeW91XG5jYW4gYmF0Y2ggdGhlbSB0b2dldGhlci4gRWFjaCBpcyBoYW5kZWQgdG8gdGhlIHJ1bnRpbWUgYXQgdGhlIHNhbWUgdGltZSxcbmFuZCBzaW5jZSBlYWNoIGNhbiBwZXJmb3JtIGFyYml0cmFyeSBvcGVyYXRpb25zIGluIHRoZSB3b3JsZCwgdGhlcmUgYXJlXG5ubyBvcmRlcmluZyBndWFyYW50ZWVzIGFib3V0IHRoZSByZXN1bHRzLlxuXG4qKk5vdGU6KiogYENtZC5ub25lYCBhbmQgYENtZC5iYXRjaCBbIENtZC5ub25lLCBDbWQubm9uZSBdYCBhbmQgYENtZC5iYXRjaCBbXWBcbmFsbCBkbyB0aGUgc2FtZSB0aGluZy5cblxuLX1cbmJhdGNoIDogQXJyYXkgKENtZCBtc2cpIC0+IENtZCBtc2dcbmJhdGNoID1cbiAgICBHcmVuLktlcm5lbC5QbGF0Zm9ybS5iYXRjaFxuXG5cblxuLS0gRkFOQ1kgU1RVRkZcblxuXG57LXwgVHJhbnNmb3JtIHRoZSBtZXNzYWdlcyBwcm9kdWNlZCBieSBhIGNvbW1hbmQuXG5WZXJ5IHNpbWlsYXIgdG8gW2BIdG1sLm1hcGBdKC9wYWNrYWdlL2dyZW4tbGFuZy9icm93c2VyL2xhdGVzdC9tb2R1bGUvSHRtbCNtYXApLlxuXG5UaGlzIGlzIHZlcnkgcmFyZWx5IHVzZWZ1bCBpbiB3ZWxsLXN0cnVjdHVyZWQgR3JlbiBjb2RlLCBzbyBkZWZpbml0ZWx5IHJlYWQgdGhlXG5zZWN0aW9uIG9uIFtzdHJ1Y3R1cmVdIGluIHRoZSBndWlkZSBiZWZvcmUgcmVhY2hpbmcgZm9yIHRoaXMhXG5cbltzdHJ1Y3R1cmVdOiBodHRwczovL2d1aWRlLmdyZW4tbGFuZy5vcmcvd2ViYXBwcy9zdHJ1Y3R1cmUuaHRtbFxuXG4tfVxubWFwIDogKGEgLT4gbXNnKSAtPiBDbWQgYSAtPiBDbWQgbXNnXG5tYXAgPVxuICAgIEdyZW4uS2VybmVsLlBsYXRmb3JtLm1hcFxuIiwKICAgICAgICAibW9kdWxlIEluaXQgZXhwb3NpbmdcbiAgICAoIFRhc2tcbiAgICAsIGF3YWl0XG4gICAgLCBhd2FpdFRhc2tcbiAgICApXG5cbnstfCBUaGlzIG1vZHVsZSBkZWZpbmVzIHRoZSBhcHAgaW5pdGlhbGl6YXRpb24gdGFzay4gVGhpcyBpcyBhIHNwZWNpYWwga2luZCBvZiB0YXNrXG50aGF0IGNhbiBvbmx5IGJlIHBhc3NlZCBhcyB0aGUgcmVzdWx0IG9mIGFuIGFwcGxpY2F0aW9uIGluaXRpYWxpemF0aW9uLiBZb3UnbGwgdHlwaWNhbGx5XG51c2UgdGhpcyBtb2R1bGUgaW4gb3JkZXIgdG8gaW5pdGlhbGl6ZSBzdWItc3lzdGVtcyBsaWtlIGBGaWxlU3lzdGVtYCBvciBgQ2hpbGRQcm9jZXNzYC5cblxuQGRvY3MgVGFzaywgYXdhaXQsIGF3YWl0VGFza1xuLX1cblxuXG5pbXBvcnQgVGFza1xuaW1wb3J0IEludGVybmFsLkluaXRcblxuXG57LXwgVGhpcyBpcyBsaWtlIGEgYFRhc2tgLCBidXQgY2FuIG9ubHkgYmUgcnVuIGFzIHBhcnQgb2YgaW5pdGlhbGl6aW5nIHlvdXJcbnByb2dyYW0uIFRoaXMgaXMgdXN1YWxseSB1c2VkIGZvciB2YWx1ZXMgd2hpY2ggc2hvdWxkIGJlIHByb3ZpZGVkIHRvIHlvdXIgcHJvZ3JhbSxcbmFuZCBvbmx5IHlvdXIgcHJvZ3JhbSwgYXMgb3Bwb3NlZCB0byB0aGlyZC1wYXJ0eSBwYWNrYWdlcy5cbi19XG50eXBlIGFsaWFzIFRhc2sgYSA9XG4gICAgSW50ZXJuYWwuSW5pdC5UYXNrIGFcblxuXG57LXwgVGhpcyBsZXQncyB5b3Ugd2FpdCBmb3IgdGhlIGNvbXBsZXRpb24gb2YgYW4gYFRhc2tgIGJlZm9yZSBlaXRoZXIgc3RhcnRpbmdcbnlvdXIgYXBwbGljYXRpb24sIG9yIGJlZ2luIGluaXRpYWxpemF0aW9uIG9mIGFub3RoZXIgYFRhc2tgLlxuXG4gICAgSW5pdC5hd2FpdCBUZXJtaW5hbC5pbml0aWFsaXplIDx8IFxcdGVybUNvbmZpZyAtPlxuICAgIEluaXQuYXdhaXQgRmlsZVN5c3RlbS5pbml0aWFsaXplIDx8IFxcZmlsZVN5c3RlbUNvbmZpZyAtPlxuICAgICAgICAtLSBTdGFydCB5b3VyIG93biBwcm9ncmFtIHdpdGggdGhlIHZhbHVlcyBmcm9tIFRlcm1pbmFsIGFuZCBGaWxlU3lzdGVtXG4gICAgICAgIE5vZGUuc3RhcnRQcm9ncmFtXG4gICAgICAgICAgICB7IG1vZGVsID0gMVxuICAgICAgICAgICAgLCBjb21tYW5kcyA9IENtZC5ub25lXG4gICAgICAgICAgICB9XG4tfVxuYXdhaXQgOiBUYXNrIGEgLT4gKGEgLT4gVGFzayBiKSAtPiBUYXNrIGJcbmF3YWl0IChJbnRlcm5hbC5Jbml0LlRhc2sgdGFzaykgZm4gPVxuICAgIEludGVybmFsLkluaXQuVGFzayAoVGFzay5hbmRUaGVuICh1bndyYXAgPDwgZm4pIHRhc2spXG5cblxuey18IFRoaXMgbGV0J3MgeW91IHdhaXQgZm9yIHRoZSBjb21wbGV0aW9uIG9mIGEgYFRhc2tgIGJlZm9yZSBlaXRoZXIgc3RhcnRpbmdcbnlvdXIgYXBwbGljYXRpb24sIG9yIGJlZ2luIGluaXRpYWxpemF0aW9uIG9mIGFub3RoZXIgYFRhc2tgLlxuXG4gICAgSW5pdC5hd2FpdCBUZXJtaW5hbC5pbml0aWFsaXplIDx8IFxcdGVybUNvbmZpZyAtPlxuICAgIEluaXQuYXdhaXRUYXNrIFRhc2subm93IDx8IFxcdGltZSAtPlxuICAgICAgICAtLSBTdGFydCB5b3VyIG93biBwcm9ncmFtIHdpdGggdGhlIHZhbHVlcyBmcm9tIFRlcm1pbmFsIGFuZCBGaWxlU3lzdGVtXG4gICAgICAgIE5vZGUuc3RhcnRQcm9ncmFtXG4gICAgICAgICAgICB7IG1vZGVsID0gdGltZVxuICAgICAgICAgICAgLCBjb21tYW5kcyA9IENtZC5ub25lXG4gICAgICAgICAgICB9XG4tfVxuYXdhaXRUYXNrIDogVGFzay5UYXNrIE5ldmVyIGEgLT4gKGEgLT4gVGFzayBiKSAtPiBUYXNrIGJcbmF3YWl0VGFzayB0YXNrIGZuID1cbiAgICBJbnRlcm5hbC5Jbml0LlRhc2sgKFRhc2suYW5kVGhlbiAodW53cmFwIDw8IGZuKSB0YXNrKVxuXG5cbnVud3JhcCA6IFRhc2sgYSAtPiBUYXNrLlRhc2sgTmV2ZXIgYVxudW53cmFwIChJbnRlcm5hbC5Jbml0LlRhc2sgdGFzaykgPVxuICAgIHRhc2tcblxuXG4iLAogICAgICAgICJlZmZlY3QgbW9kdWxlIFRpbWUgd2hlcmUgeyBzdWJzY3JpcHRpb24gPSBNeVN1YiB9IGV4cG9zaW5nXG4gICggUG9zaXhcbiAgLCBub3dcbiAgLCBldmVyeVxuICAsIHBvc2l4VG9NaWxsaXNcbiAgLCBtaWxsaXNUb1Bvc2l4XG4gICwgWm9uZVxuICAsIHV0Y1xuICAsIGhlcmVcbiAgLCB0b1llYXJcbiAgLCB0b01vbnRoXG4gICwgdG9EYXlcbiAgLCB0b1dlZWtkYXlcbiAgLCB0b0hvdXJcbiAgLCB0b01pbnV0ZVxuICAsIHRvU2Vjb25kXG4gICwgdG9NaWxsaXNcbiAgLCBNb250aCguLilcbiAgLCBXZWVrZGF5KC4uKVxuICAsIGN1c3RvbVpvbmVcbiAgLCBnZXRab25lTmFtZVxuICAsIFpvbmVOYW1lKC4uKVxuICApXG5cblxuey18IEZ1bmN0aW9ucyBmb3Igd29ya2luZyB3aXRoIHRpbWUgYW5kIHRpbWUgem9uZXMuXG5cbkBkb2NzIFBvc2l4LCBub3csIGV2ZXJ5LCBwb3NpeFRvTWlsbGlzLCBtaWxsaXNUb1Bvc2l4XG5cbiMjIFRpbWUgWm9uZXNcbkBkb2NzIFpvbmUsIHV0YywgaGVyZVxuXG4jIyBIdW1hbiBUaW1lc1xuQGRvY3MgdG9ZZWFyLCB0b01vbnRoLCB0b0RheSwgdG9XZWVrZGF5LCB0b0hvdXIsIHRvTWludXRlLCB0b1NlY29uZCwgdG9NaWxsaXNcblxuIyMgV2Vla3MgYW5kIE1vbnRoc1xuQGRvY3MgV2Vla2RheSwgTW9udGhcblxuIyMgRm9yIFBhY2thZ2UgQXV0aG9yc1xuQGRvY3MgY3VzdG9tWm9uZSwgZ2V0Wm9uZU5hbWUsIFpvbmVOYW1lXG5cbi19XG5cblxuaW1wb3J0IEFycmF5IGV4cG9zaW5nIChBcnJheSlcbmltcG9ydCBCYXNpY3MgZXhwb3NpbmcgKC4uKVxuaW1wb3J0IERpY3RcbmltcG9ydCBNYXRoXG5pbXBvcnQgTWF5YmUgZXhwb3NpbmcgKE1heWJlKC4uKSlcbmltcG9ydCBQbGF0Zm9ybVxuaW1wb3J0IFBsYXRmb3JtLlN1YiBleHBvc2luZyAoU3ViKVxuaW1wb3J0IFByb2Nlc3NcbmltcG9ydCBTdHJpbmcgZXhwb3NpbmcgKFN0cmluZylcbmltcG9ydCBUYXNrIGV4cG9zaW5nIChUYXNrKVxuaW1wb3J0IEdyZW4uS2VybmVsLlRpbWVcblxuXG5cbi0tIFBPU0lYXG5cblxuey18IEEgY29tcHV0ZXIgcmVwcmVzZW50YXRpb24gb2YgdGltZS4gSXQgaXMgdGhlIHNhbWUgYWxsIG92ZXIgRWFydGgsIHNvIGlmIHdlXG5oYXZlIGEgcGhvbmUgY2FsbCBvciBtZWV0aW5nIGF0IGEgY2VydGFpbiBQT1NJWCB0aW1lLCB0aGVyZSBpcyBubyBhbWJpZ3VpdHkuXG5cbkl0IGlzIHZlcnkgaGFyZCBmb3IgaHVtYW5zIHRvIF9yZWFkXyBhIFBPU0lYIHRpbWUgdGhvdWdoLCBzbyB3ZSB1c2UgZnVuY3Rpb25zXG5saWtlIFtgdG9Ib3VyYF0oI3RvSG91cikgYW5kIFtgdG9NaW51dGVgXSgjdG9NaW51dGUpIHRvIGB2aWV3YCB0aGVtLlxuLX1cbnR5cGUgUG9zaXggPSBQb3NpeCBJbnRcblxuXG57LXwgR2V0IHRoZSBQT1NJWCB0aW1lIGF0IHRoZSBtb21lbnQgd2hlbiB0aGlzIHRhc2sgaXMgcnVuLlxuLX1cbm5vdyA6IFRhc2sgeCBQb3NpeFxubm93ID1cbiAgR3Jlbi5LZXJuZWwuVGltZS5ub3cgbWlsbGlzVG9Qb3NpeFxuXG5cbnstfCBUdXJuIGEgYFBvc2l4YCB0aW1lIGludG8gdGhlIG51bWJlciBvZiBtaWxsaXNlY29uZHMgc2luY2UgMTk3MCBKYW51YXJ5IDFcbmF0IDAwOjAwOjAwIFVUQy4gSXQgd2FzIGEgVGh1cnNkYXkuXG4tfVxucG9zaXhUb01pbGxpcyA6IFBvc2l4IC0+IEludFxucG9zaXhUb01pbGxpcyAoUG9zaXggbWlsbGlzKSA9XG4gIG1pbGxpc1xuXG5cbnstfCBUdXJuIG1pbGxpc2Vjb25kcyBpbnRvIGEgYFBvc2l4YCB0aW1lLlxuLX1cbm1pbGxpc1RvUG9zaXggOiBJbnQgLT4gUG9zaXhcbm1pbGxpc1RvUG9zaXggPVxuICBQb3NpeFxuXG5cblxuLS0gVElNRSBaT05FU1xuXG5cbnstfCBJbmZvcm1hdGlvbiBhYm91dCBhIHBhcnRpY3VsYXIgdGltZSB6b25lLlxuXG5UaGUgW0lBTkEgVGltZSBab25lIERhdGFiYXNlXVtpYW5hXSB0cmFja3MgdGhpbmdzIGxpa2UgVVRDIG9mZnNldHMgYW5kXG5kYXlsaWdodC1zYXZpbmcgcnVsZXMgc28gdGhhdCB5b3UgY2FuIHR1cm4gYSBgUG9zaXhgIHRpbWUgaW50byBsb2NhbCB0aW1lc1xud2l0aGluIGEgdGltZSB6b25lLlxuXG5TZWUgW2B1dGNgXSgjdXRjKSBhbmQgW2BoZXJlYF0oI2hlcmUpIHRvIGxlYXJuIGhvdyB0byBvYnRhaW4gYFpvbmVgIHZhbHVlcy5cblxuW2lhbmFdOiBodHRwczovL3d3dy5pYW5hLm9yZy90aW1lLXpvbmVzXG4tfVxudHlwZSBab25lID1cbiAgWm9uZSBJbnQgKEFycmF5IEVyYSlcblxuXG4tLSBUT0RPOiBhZGQgdGhpcyBub3RlIGJhY2sgdG8gYFpvbmVgIGRvY3Mgd2hlbiBpdCBpcyB0cnVlXG4tLVxuLS0gRGlkIHlvdSBrbm93IHRoYXQgaW4gQ2FsaWZvcm5pYSB0aGUgdGltZXMgY2hhbmdlIGZyb20gM3BtIFBTVCB0byAzcG0gUERUIHRvXG4tLSBjYXB0dXJlIHdoZXRoZXIgaXQgaXMgZGF5bGlnaHQtc2F2aW5nIHRpbWU/IFRoZSBkYXRhYmFzZSB0cmFja3MgdGhvc2Vcbi0tIGFiYnJldmlhdGlvbiBjaGFuZ2VzIHRvby4gKFRvbnMgb2YgdGltZSB6b25lcyBkbyB0aGF0IGFjdHVhbGx5Lilcbi0tXG5cblxuey18IEN1cnJlbnRseSB0aGUgcHVibGljIEFQSSBvbmx5IG5lZWRzOlxuXG4tIGBzdGFydGAgaXMgdGhlIGJlZ2lubmluZyBvZiB0aGlzIGBFcmFgIGluIFwibWludXRlcyBzaW5jZSB0aGUgVW5peCBFcG9jaFwiXG4tIGBvZmZzZXRgIGlzIHRoZSBVVEMgb2Zmc2V0IG9mIHRoaXMgYEVyYWAgaW4gbWludXRlc1xuXG5CdXQgZXZlbnR1YWxseSwgaXQgd2lsbCBtYWtlIHNlbnNlIHRvIGhhdmUgYGFiYnIgOiBTdHJpbmdgIGZvciBgUFNUYCB2cyBgUERUYFxuLX1cbnR5cGUgYWxpYXMgRXJhID1cbiAgeyBzdGFydCA6IEludFxuICAsIG9mZnNldCA6IEludFxuICB9XG5cblxuey18IFRoZSB0aW1lIHpvbmUgZm9yIENvb3JkaW5hdGVkIFVuaXZlcnNhbCBUaW1lIChbVVRDXVtdKVxuXG5UaGUgYHV0Y2Agem9uZSBoYXMgbm8gdGltZSBhZGp1c3RtZW50cy4gSXQgbmV2ZXIgb2JzZXJ2ZXMgZGF5bGlnaHQtc2F2aW5nXG50aW1lIGFuZCBpdCBuZXZlciBzaGlmdHMgYXJvdW5kIGJhc2VkIG9uIHBvbGl0aWNhbCByZXN0cnVjdHVyaW5nLlxuXG5bVVRDXTogaHR0cHM6Ly9lbi53aWtpcGVkaWEub3JnL3dpa2kvQ29vcmRpbmF0ZWRfVW5pdmVyc2FsX1RpbWVcbi19XG51dGMgOiBab25lXG51dGMgPVxuICBab25lIDAgW11cblxuXG57LXwgUHJvZHVjZSBhIGBab25lYCBiYXNlZCBvbiB0aGUgY3VycmVudCBVVEMgb2Zmc2V0LiBZb3UgY2FuIHVzZSB0aGlzIHRvIGZpZ3VyZVxub3V0IHdoYXQgZGF5IGl0IGlzIHdoZXJlIHlvdSBhcmU6XG5cbiAgICBpbXBvcnQgVGFzayBleHBvc2luZyAoVGFzaylcbiAgICBpbXBvcnQgVGltZVxuXG4gICAgd2hhdERheUlzSXQgOiBUYXNrIHggSW50XG4gICAgd2hhdERheUlzSXQgPVxuICAgICAgVGFzay5tYXAyIFRpbWUudG9EYXkgVGltZS5oZXJlIFRpbWUubm93XG5cbioqQWNjdXJhY3kgTm90ZToqKiBUaGlzIGZ1bmN0aW9uIGNhbiBvbmx5IGdpdmUgdGltZSB6b25lcyBsaWtlIGBFdGMvR01UKzlgIG9yXG5gRXRjL0dNVC02YC4gSXQgY2Fubm90IGdpdmUgeW91IGBFdXJvcGUvU3RvY2tob2xtYCwgYEFzaWEvVG9reW9gLCBvciBhbnkgb3RoZXJcbm5vcm1hbCB0aW1lIHpvbmUgZnJvbSB0aGUgW2Z1bGwgbGlzdF1bdHpdIGR1ZSB0byBsaW1pdGF0aW9ucyBpbiBKYXZhU2NyaXB0LlxuRm9yIGV4YW1wbGUsIGlmIHlvdSBydW4gYGhlcmVgIGluIE5ldyBZb3JrIENpdHksIHRoZSByZXN1bHRpbmcgYFpvbmVgIHdpbGxcbm5ldmVyIGJlIGBBbWVyaWNhL05ld19Zb3JrYC4gSW5zdGVhZCB5b3UgZ2V0IGBFdGMvR01ULTVgIG9yIGBFdGMvR01ULTRgXG5kZXBlbmRpbmcgb24gRGF5bGlnaHQgU2F2aW5nIFRpbWUuIFNvIGV2ZW4gdGhvdWdoIGJyb3dzZXJzIG11c3QgaGF2ZSBpbnRlcm5hbFxuYWNjZXNzIHRvIGBBbWVyaWNhL05ld19Zb3JrYCB0byBmaWd1cmUgb3V0IHRoYXQgb2Zmc2V0LCB0aGVyZSBpcyBubyBwdWJsaWMgQVBJXG50byBnZXQgdGhlIGZ1bGwgaW5mb3JtYXRpb24uIFRoaXMgbWVhbnMgdGhlIGBab25lYCB5b3UgZ2V0IGZyb20gdGhpcyBmdW5jdGlvblxud2lsbCBhY3Qgd2VpcmQgaWYgKDEpIGFuIGFwcGxpY2F0aW9uIHN0YXlzIG9wZW4gYWNyb3NzIGEgRGF5bGlnaHQgU2F2aW5nIFRpbWVcbmJvdW5kYXJ5IG9yICgyKSB5b3UgdHJ5IHRvIHVzZSBpdCBvbiBoaXN0b3JpY2FsIGRhdGEuXG5cbioqRnV0dXJlIE5vdGU6KiogV2UgY2FuIGltcHJvdmUgYGhlcmVgIHdoZW4gdGhlcmUgaXMgZ29vZCBicm93c2VyIHN1cHBvcnQgZm9yXG5KYXZhU2NyaXB0IGZ1bmN0aW9ucyB0aGF0ICgxKSBleHBvc2UgdGhlIElBTkEgdGltZSB6b25lIGRhdGFiYXNlIGFuZCAoMikgbGV0XG55b3UgYXNrIHRoZSB0aW1lIHpvbmUgb2YgdGhlIGNvbXB1dGVyLiBUaGUgY29tbWl0dGVlIHRoYXQgcmV2aWV3cyBhZGRpdGlvbnMgdG9cbkphdmFTY3JpcHQgaXMgY2FsbGVkIFRDMzksIGFuZCBJIGVuY291cmFnZSB5b3UgdG8gcHVzaCBmb3IgdGhlc2UgY2FwYWJpbGl0aWVzISBJXG5jYW5ub3QgZG8gaXQgbXlzZWxmIHVuZm9ydHVuYXRlbHkuXG5cbioqQWx0ZXJuYXRpdmVzOioqIFNlZSB0aGUgYGN1c3RvbVpvbmVgIGRvY3MgdG8gbGVhcm4gaG93IHRvIGltcGxlbWVudCBzdG9wZ2Fwcy5cblxuW3R6XTogaHR0cHM6Ly9lbi53aWtpcGVkaWEub3JnL3dpa2kvTGlzdF9vZl90el9kYXRhYmFzZV90aW1lX3pvbmVzXG4tfVxuaGVyZSA6IFRhc2sgeCBab25lXG5oZXJlID1cbiAgR3Jlbi5LZXJuZWwuVGltZS5oZXJlIHt9XG5cblxuXG4tLSBEQVRFU1xuXG5cbnstfCBXaGF0IHllYXIgaXMgaXQ/IVxuXG4gICAgaW1wb3J0IFRpbWUgZXhwb3NpbmcgKHRvWWVhciwgdXRjLCBtaWxsaXNUb1Bvc2l4KVxuXG4gICAgdG9ZZWFyIHV0YyAobWlsbGlzVG9Qb3NpeCAwKSA9PSAxOTcwXG4gICAgdG9ZZWFyIG55YyAobWlsbGlzVG9Qb3NpeCAwKSA9PSAxOTY5XG5cbiAgICAtLSBwcmV0ZW5kIGBueWNgIGlzIHRoZSBgWm9uZWAgZm9yIEFtZXJpY2EvTmV3X1lvcmsuXG4tfVxudG9ZZWFyIDogWm9uZSAtPiBQb3NpeCAtPiBJbnRcbnRvWWVhciB6b25lIHRpbWUgPVxuICAodG9DaXZpbCAodG9BZGp1c3RlZE1pbnV0ZXMgem9uZSB0aW1lKSkueWVhclxuXG5cbnstfCBXaGF0IG1vbnRoIGlzIGl0PyFcblxuICAgIGltcG9ydCBUaW1lIGV4cG9zaW5nICh0b01vbnRoLCB1dGMsIG1pbGxpc1RvUG9zaXgpXG5cbiAgICB0b01vbnRoIHV0YyAobWlsbGlzVG9Qb3NpeCAwKSA9PSBKYW5cbiAgICB0b01vbnRoIG55YyAobWlsbGlzVG9Qb3NpeCAwKSA9PSBEZWNcblxuICAgIC0tIHByZXRlbmQgYG55Y2AgaXMgdGhlIGBab25lYCBmb3IgQW1lcmljYS9OZXdfWW9yay5cbi19XG50b01vbnRoIDogWm9uZSAtPiBQb3NpeCAtPiBNb250aFxudG9Nb250aCB6b25lIHRpbWUgPVxuICBjYXNlICh0b0NpdmlsICh0b0FkanVzdGVkTWludXRlcyB6b25lIHRpbWUpKS5tb250aCBvZlxuICAgIDEgIC0+IEphblxuICAgIDIgIC0+IEZlYlxuICAgIDMgIC0+IE1hclxuICAgIDQgIC0+IEFwclxuICAgIDUgIC0+IE1heVxuICAgIDYgIC0+IEp1blxuICAgIDcgIC0+IEp1bFxuICAgIDggIC0+IEF1Z1xuICAgIDkgIC0+IFNlcFxuICAgIDEwIC0+IE9jdFxuICAgIDExIC0+IE5vdlxuICAgIF8gIC0+IERlY1xuXG5cbnstfCBXaGF0IGRheSBpcyBpdD8hIChEYXlzIGdvIGZyb20gMSB0byAzMSlcblxuICAgIGltcG9ydCBUaW1lIGV4cG9zaW5nICh0b0RheSwgdXRjLCBtaWxsaXNUb1Bvc2l4KVxuXG4gICAgdG9EYXkgdXRjIChtaWxsaXNUb1Bvc2l4IDApID09IDFcbiAgICB0b0RheSBueWMgKG1pbGxpc1RvUG9zaXggMCkgPT0gMzFcblxuICAgIC0tIHByZXRlbmQgYG55Y2AgaXMgdGhlIGBab25lYCBmb3IgQW1lcmljYS9OZXdfWW9yay5cblxuLX1cbnRvRGF5IDogWm9uZSAtPiBQb3NpeCAtPiBJbnRcbnRvRGF5IHpvbmUgdGltZSA9XG4gICh0b0NpdmlsICh0b0FkanVzdGVkTWludXRlcyB6b25lIHRpbWUpKS5kYXlcblxuXG57LXwgV2hhdCBkYXkgb2YgdGhlIHdlZWsgaXMgaXQ/XG5cbiAgICBpbXBvcnQgVGltZSBleHBvc2luZyAodG9XZWVrZGF5LCB1dGMsIG1pbGxpc1RvUG9zaXgpXG5cbiAgICB0b1dlZWtkYXkgdXRjIChtaWxsaXNUb1Bvc2l4IDApID09IFRodVxuICAgIHRvV2Vla2RheSBueWMgKG1pbGxpc1RvUG9zaXggMCkgPT0gV2VkXG5cbiAgICAtLSBwcmV0ZW5kIGBueWNgIGlzIHRoZSBgWm9uZWAgZm9yIEFtZXJpY2EvTmV3X1lvcmsuXG4tfVxudG9XZWVrZGF5IDogWm9uZSAtPiBQb3NpeCAtPiBXZWVrZGF5XG50b1dlZWtkYXkgem9uZSB0aW1lID1cbiAgY2FzZSBNYXRoLm1vZEJ5IDcgKGZsb29yZWREaXYgKHRvQWRqdXN0ZWRNaW51dGVzIHpvbmUgdGltZSkgKDYwICogMjQpKSBvZlxuICAgIDAgLT4gVGh1XG4gICAgMSAtPiBGcmlcbiAgICAyIC0+IFNhdFxuICAgIDMgLT4gU3VuXG4gICAgNCAtPiBNb25cbiAgICA1IC0+IFR1ZVxuICAgIF8gLT4gV2VkXG5cblxuey18IFdoYXQgaG91ciBpcyBpdD8gKEZyb20gMCB0byAyMylcblxuICAgIGltcG9ydCBUaW1lIGV4cG9zaW5nICh0b0hvdXIsIHV0YywgbWlsbGlzVG9Qb3NpeClcblxuICAgIHRvSG91ciB1dGMgKG1pbGxpc1RvUG9zaXggMCkgPT0gMCAgLS0gMTJhbVxuICAgIHRvSG91ciBueWMgKG1pbGxpc1RvUG9zaXggMCkgPT0gMTkgLS0gN3BtXG5cbiAgICAtLSBwcmV0ZW5kIGBueWNgIGlzIHRoZSBgWm9uZWAgZm9yIEFtZXJpY2EvTmV3X1lvcmsuXG4tfVxudG9Ib3VyIDogWm9uZSAtPiBQb3NpeCAtPiBJbnRcbnRvSG91ciB6b25lIHRpbWUgPVxuICBNYXRoLm1vZEJ5IDI0IChmbG9vcmVkRGl2ICh0b0FkanVzdGVkTWludXRlcyB6b25lIHRpbWUpIDYwKVxuXG5cbnstfCBXaGF0IG1pbnV0ZSBpcyBpdD8gKEZyb20gMCB0byA1OSlcblxuICAgIGltcG9ydCBUaW1lIGV4cG9zaW5nICh0b01pbnV0ZSwgdXRjLCBtaWxsaXNUb1Bvc2l4KVxuXG4gICAgdG9NaW51dGUgdXRjIChtaWxsaXNUb1Bvc2l4IDApID09IDBcblxuVGhpcyBjYW4gYmUgZGlmZmVyZW50IGluIGRpZmZlcmVudCB0aW1lIHpvbmVzLiBTb21lIHRpbWUgem9uZXMgYXJlIG9mZnNldFxuYnkgMzAgb3IgNDUgbWludXRlcyFcbi19XG50b01pbnV0ZSA6IFpvbmUgLT4gUG9zaXggLT4gSW50XG50b01pbnV0ZSB6b25lIHRpbWUgPVxuICBNYXRoLm1vZEJ5IDYwICh0b0FkanVzdGVkTWludXRlcyB6b25lIHRpbWUpXG5cblxuey18IFdoYXQgc2Vjb25kIGlzIGl0P1xuXG4gICAgaW1wb3J0IFRpbWUgZXhwb3NpbmcgKHRvU2Vjb25kLCB1dGMsIG1pbGxpc1RvUG9zaXgpXG5cbiAgICB0b1NlY29uZCB1dGMgKG1pbGxpc1RvUG9zaXggICAgMCkgPT0gMFxuICAgIHRvU2Vjb25kIHV0YyAobWlsbGlzVG9Qb3NpeCAxMjM0KSA9PSAxXG4gICAgdG9TZWNvbmQgdXRjIChtaWxsaXNUb1Bvc2l4IDU2NzgpID09IDVcbi19XG50b1NlY29uZCA6IFpvbmUgLT4gUG9zaXggLT4gSW50XG50b1NlY29uZCBfIHRpbWUgPVxuICBNYXRoLm1vZEJ5IDYwIChmbG9vcmVkRGl2IChwb3NpeFRvTWlsbGlzIHRpbWUpIDEwMDApXG5cblxuey18XG4gICAgaW1wb3J0IFRpbWUgZXhwb3NpbmcgKHRvTWlsbGlzLCB1dGMsIG1pbGxpc1RvUG9zaXgpXG5cbiAgICB0b01pbGxpcyB1dGMgKG1pbGxpc1RvUG9zaXggICAgMCkgPT0gMFxuICAgIHRvTWlsbGlzIHV0YyAobWlsbGlzVG9Qb3NpeCAxMjM0KSA9PSAyMzRcbiAgICB0b01pbGxpcyB1dGMgKG1pbGxpc1RvUG9zaXggNTY3OCkgPT0gNjc4XG4tfVxudG9NaWxsaXMgOiBab25lIC0+IFBvc2l4IC0+IEludFxudG9NaWxsaXMgXyB0aW1lID1cbiAgTWF0aC5tb2RCeSAxMDAwIChwb3NpeFRvTWlsbGlzIHRpbWUpXG5cblxuXG4tLSBEQVRFIEhFTFBFUlNcblxuXG50b0FkanVzdGVkTWludXRlcyA6IFpvbmUgLT4gUG9zaXggLT4gSW50XG50b0FkanVzdGVkTWludXRlcyAoWm9uZSBkZWZhdWx0T2Zmc2V0IGVyYXMpIHRpbWUgPVxuICB0b0FkanVzdGVkTWludXRlc0hlbHAgZGVmYXVsdE9mZnNldCAoZmxvb3JlZERpdiAocG9zaXhUb01pbGxpcyB0aW1lKSA2MDAwMCkgZXJhc1xuXG5cbnRvQWRqdXN0ZWRNaW51dGVzSGVscCA6IEludCAtPiBJbnQgLT4gQXJyYXkgRXJhIC0+IEludFxudG9BZGp1c3RlZE1pbnV0ZXNIZWxwIGRlZmF1bHRPZmZzZXQgcG9zaXhNaW51dGVzIGVyYXMgPVxuICBjYXNlIEFycmF5LmdldCAwIGVyYXMgb2ZcbiAgICBOb3RoaW5nIC0+XG4gICAgICBwb3NpeE1pbnV0ZXMgKyBkZWZhdWx0T2Zmc2V0XG5cbiAgICBKdXN0IGVyYSAtPlxuICAgICAgaWYgZXJhLnN0YXJ0IDwgcG9zaXhNaW51dGVzIHRoZW5cbiAgICAgICAgcG9zaXhNaW51dGVzICsgZXJhLm9mZnNldFxuICAgICAgZWxzZVxuICAgICAgICB0b0FkanVzdGVkTWludXRlc0hlbHAgZGVmYXVsdE9mZnNldCBwb3NpeE1pbnV0ZXMgKEFycmF5LnNsaWNlIDEgKEFycmF5Lmxlbmd0aCBlcmFzKSBlcmFzKVxuXG5cbnRvQ2l2aWwgOiBJbnQgLT4geyB5ZWFyIDogSW50LCBtb250aCA6IEludCwgZGF5IDogSW50IH1cbnRvQ2l2aWwgbWludXRlcyA9XG4gIGxldFxuICAgIHJhd0RheSAgICA9IGZsb29yZWREaXYgbWludXRlcyAoNjAgKiAyNCkgKyA3MTk0NjhcbiAgICBlcmEgICAgICAgPSAoaWYgcmF3RGF5ID49IDAgdGhlbiByYXdEYXkgZWxzZSByYXdEYXkgLSAxNDYwOTYpIC8vIDE0NjA5N1xuICAgIGRheU9mRXJhICA9IHJhd0RheSAtIGVyYSAqIDE0NjA5NyAtLSBbMCwgMTQ2MDk2XVxuICAgIHllYXJPZkVyYSA9IChkYXlPZkVyYSAtIGRheU9mRXJhIC8vIDE0NjAgKyBkYXlPZkVyYSAvLyAzNjUyNCAtIGRheU9mRXJhIC8vIDE0NjA5NikgLy8gMzY1IC0tIFswLCAzOTldXG4gICAgeWVhciAgICAgID0geWVhck9mRXJhICsgZXJhICogNDAwXG4gICAgZGF5T2ZZZWFyID0gZGF5T2ZFcmEgLSAoMzY1ICogeWVhck9mRXJhICsgeWVhck9mRXJhIC8vIDQgLSB5ZWFyT2ZFcmEgLy8gMTAwKSAtLSBbMCwgMzY1XVxuICAgIG1wICAgICAgICA9ICg1ICogZGF5T2ZZZWFyICsgMikgLy8gMTUzIC0tIFswLCAxMV1cbiAgICBtb250aCAgICAgPSBtcCArIChpZiBtcCA8IDEwIHRoZW4gMyBlbHNlIC05KSAtLSBbMSwgMTJdXG4gIGluXG4gIHsgeWVhciA9IHllYXIgKyAoaWYgbW9udGggPD0gMiB0aGVuIDEgZWxzZSAwKVxuICAsIG1vbnRoID0gbW9udGhcbiAgLCBkYXkgPSBkYXlPZlllYXIgLSAoMTUzICogbXAgKyAyKSAvLyA1ICsgMSAtLSBbMSwgMzFdXG4gIH1cblxuXG5mbG9vcmVkRGl2IDogSW50IC0+IEZsb2F0IC0+IEludFxuZmxvb3JlZERpdiBudW1lcmF0b3IgZGVub21pbmF0b3IgPVxuICBNYXRoLmZsb29yICh0b0Zsb2F0IG51bWVyYXRvciAvIGRlbm9taW5hdG9yKVxuXG5cblxuLS0gV0VFS0RBWVMgQU5EIE1PTlRIU1xuXG5cbnstfCBSZXByZXNlbnRzIGEgYFdlZWtkYXlgIHNvIHRoYXQgeW91IGNhbiBjb252ZXJ0IGl0IHRvIGEgYFN0cmluZ2Agb3IgYEludGBcbmhvd2V2ZXIgeW91IHBsZWFzZS4gRm9yIGV4YW1wbGUsIGlmIHlvdSBuZWVkIHRoZSBKYXBhbmVzZSByZXByZXNlbnRhdGlvbiwgeW91XG5jYW4gc2F5OlxuXG4gICAgdG9KYXBhbmVzZVdlZWtkYXkgOiBXZWVrZGF5IC0+IFN0cmluZ1xuICAgIHRvSmFwYW5lc2VXZWVrZGF5IHdlZWtkYXkgPVxuICAgICAgY2FzZSB3ZWVrZGF5IG9mXG4gICAgICAgIE1vbiAtPiBcIuaciFwiXG4gICAgICAgIFR1ZSAtPiBcIueBq1wiXG4gICAgICAgIFdlZCAtPiBcIuawtFwiXG4gICAgICAgIFRodSAtPiBcIuacqFwiXG4gICAgICAgIEZyaSAtPiBcIumHkVwiXG4gICAgICAgIFNhdCAtPiBcIuWcn1wiXG4gICAgICAgIFN1biAtPiBcIuaXpVwiXG4tfVxudHlwZSBXZWVrZGF5ID0gTW9uIHwgVHVlIHwgV2VkIHwgVGh1IHwgRnJpIHwgU2F0IHwgU3VuXG5cblxuey18IFJlcHJlc2VudHMgYSBgTW9udGhgIHNvIHRoYXQgeW91IGNhbiBjb252ZXJ0IGl0IHRvIGEgYFN0cmluZ2Agb3IgYEludGBcbmhvd2V2ZXIgeW91IHBsZWFzZS4gRm9yIGV4YW1wbGUsIGlmIHlvdSBuZWVkIHRoZSBEYW5pc2ggcmVwcmVzZW50YXRpb24sIHlvdVxuY2FuIHNheTpcblxuICAgIHRvRGFuaXNoTW9udGggOiBNb250aCAtPiBTdHJpbmdcbiAgICB0b0RhbmlzaE1vbnRoIG1vbnRoID1cbiAgICAgIGNhc2UgbW9udGggb2ZcbiAgICAgICAgSmFuIC0+IFwiamFudWFyXCJcbiAgICAgICAgRmViIC0+IFwiZmVicnVhclwiXG4gICAgICAgIE1hciAtPiBcIm1hcnRzXCJcbiAgICAgICAgQXByIC0+IFwiYXByaWxcIlxuICAgICAgICBNYXkgLT4gXCJtYWpcIlxuICAgICAgICBKdW4gLT4gXCJqdW5pXCJcbiAgICAgICAgSnVsIC0+IFwianVsaVwiXG4gICAgICAgIEF1ZyAtPiBcImF1Z3VzdFwiXG4gICAgICAgIFNlcCAtPiBcInNlcHRlbWJlclwiXG4gICAgICAgIE9jdCAtPiBcIm9rdG9iZXJcIlxuICAgICAgICBOb3YgLT4gXCJub3ZlbWJlclwiXG4gICAgICAgIERlYyAtPiBcImRlY2VtYmVyXCJcbi19XG50eXBlIE1vbnRoID0gSmFuIHwgRmViIHwgTWFyIHwgQXByIHwgTWF5IHwgSnVuIHwgSnVsIHwgQXVnIHwgU2VwIHwgT2N0IHwgTm92IHwgRGVjXG5cblxuXG4tLSBTVUJTQ1JJUFRJT05TXG5cblxuey18IEdldCB0aGUgY3VycmVudCB0aW1lIHBlcmlvZGljYWxseS4gSG93IG9mdGVuIHRob3VnaD8gV2VsbCwgeW91IHByb3ZpZGUgYW5cbmludGVydmFsIGluIG1pbGxpc2Vjb25kcyAobGlrZSBgMTAwMGAgZm9yIGEgc2Vjb25kIG9yIGA2MCAqIDEwMDBgIGZvciBhIG1pbnV0ZVxub3IgYDYwICogNjAgKiAxMDAwYCBmb3IgYW4gaG91cikgYW5kIHRoYXQgaXMgaG93IG9mdGVuIHlvdSBnZXQgYSBuZXcgdGltZSFcblxuQ2hlY2sgb3V0IFt0aGlzIGV4YW1wbGVdKGh0dHBzOi8vZ3Jlbi1sYW5nLm9yZy9leGFtcGxlcy90aW1lKSB0byBzZWUgaG93IHRvIHVzZVxuaXQgaW4gYW4gYXBwbGljYXRpb24uXG5cbioqVGhpcyBmdW5jdGlvbiBpcyBub3QgZm9yIGFuaW1hdGlvbi4qKiBVc2UgdGhlIFtgb25BbmltYXRpb25GcmFtZWBdW2FmXVxuZnVuY3Rpb24gZm9yIHRoYXQgc29ydCBvZiB0aGluZyEgSXQgc3luY3MgdXAgd2l0aCByZXBhaW50cyBhbmQgd2lsbCBlbmQgdXBcbmJlaW5nIG11Y2ggc21vb3RoZXIgZm9yIGFueSBtb3ZpbmcgdmlzdWFscy5cblxuW2FmXTogL3BhY2thZ2VzL2dyZW4tbGFuZy9icm93c2VyL2xhdGVzdC9Ccm93c2VyLUV2ZW50cyNvbkFuaW1hdGlvbkZyYW1lXG4tfVxuZXZlcnkgOiBGbG9hdCAtPiAoUG9zaXggLT4gbXNnKSAtPiBTdWIgbXNnXG5ldmVyeSBpbnRlcnZhbCB0YWdnZXIgPVxuICBzdWJzY3JpcHRpb24gKEV2ZXJ5IGludGVydmFsIHRhZ2dlcilcblxuXG50eXBlIE15U3ViIG1zZyA9XG4gIEV2ZXJ5IEZsb2F0IChQb3NpeCAtPiBtc2cpXG5cblxuc3ViTWFwIDogKGEgLT4gYikgLT4gTXlTdWIgYSAtPiBNeVN1YiBiXG5zdWJNYXAgZiAoRXZlcnkgaW50ZXJ2YWwgdGFnZ2VyKSA9XG4gIEV2ZXJ5IGludGVydmFsIChmIDw8IHRhZ2dlcilcblxuXG5cbi0tIEVGRkVDVCBNQU5BR0VSXG5cblxudHlwZSBhbGlhcyBTdGF0ZSBtc2cgPVxuICB7IHRhZ2dlcnMgOiBUYWdnZXJzIG1zZ1xuICAsIHByb2Nlc3NlcyA6IFByb2Nlc3Nlc1xuICB9XG5cblxudHlwZSBhbGlhcyBQcm9jZXNzZXMgPVxuICBEaWN0LkRpY3QgRmxvYXQgUGxhdGZvcm0uUHJvY2Vzc0lkXG5cblxudHlwZSBhbGlhcyBUYWdnZXJzIG1zZyA9XG4gIERpY3QuRGljdCBGbG9hdCAoQXJyYXkgKFBvc2l4IC0+IG1zZykpXG5cblxuaW5pdCA6IFRhc2sgTmV2ZXIgKFN0YXRlIG1zZylcbmluaXQgPVxuICBUYXNrLnN1Y2NlZWQgeyB0YWdnZXJzID0gRGljdC5lbXB0eSwgcHJvY2Vzc2VzID0gRGljdC5lbXB0eSB9XG5cblxub25FZmZlY3RzIDogUGxhdGZvcm0uUm91dGVyIG1zZyBGbG9hdCAtPiBBcnJheSAoTXlTdWIgbXNnKSAtPiBTdGF0ZSBtc2cgLT4gVGFzayBOZXZlciAoU3RhdGUgbXNnKVxub25FZmZlY3RzIHJvdXRlciBzdWJzIHtwcm9jZXNzZXN9ID1cbiAgbGV0XG4gICAgbmV3VGFnZ2VycyA9XG4gICAgICBBcnJheS5mb2xkbCBhZGRNeVN1YiBEaWN0LmVtcHR5IHN1YnNcblxuICAgIHNwYXduQXJyYXkgPVxuICAgICAgICBEaWN0LmRpZmYgbmV3VGFnZ2VycyBwcm9jZXNzZXNcbiAgICAgICAgICAgIHw+IERpY3Qua2V5c1xuXG4gICAgZXhpc3RpbmdEaWN0ID1cbiAgICAgICAgRGljdC5maWx0ZXIgKFxca2V5IF8gLT4gRGljdC5tZW1iZXIga2V5IG5ld1RhZ2dlcnMpIHByb2Nlc3Nlc1xuXG4gICAga2lsbFRhc2sgPVxuICAgICAgICBEaWN0LmRpZmYgcHJvY2Vzc2VzIG5ld1RhZ2dlcnNcbiAgICAgICAgICAgIHw+IERpY3QudmFsdWVzXG4gICAgICAgICAgICB8PiBBcnJheS5mb2xkbCAoXFxpZCAtPiBUYXNrLmFuZFRoZW4gKFxcXyAtPiBQcm9jZXNzLmtpbGwgaWQpKSAoVGFzay5zdWNjZWVkIHt9KVxuICBpblxuICAgIGtpbGxUYXNrXG4gICAgICB8PiBUYXNrLmFuZFRoZW4gKFxcXyAtPiBzcGF3bkhlbHAgcm91dGVyIHNwYXduQXJyYXkgZXhpc3RpbmdEaWN0KVxuICAgICAgfD4gVGFzay5hbmRUaGVuIChcXG5ld1Byb2Nlc3NlcyAtPiBUYXNrLnN1Y2NlZWQgeyB0YWdnZXJzID0gbmV3VGFnZ2VycywgcHJvY2Vzc2VzID0gbmV3UHJvY2Vzc2VzIH0pXG5cblxuYWRkTXlTdWIgOiBNeVN1YiBtc2cgLT4gVGFnZ2VycyBtc2cgLT4gVGFnZ2VycyBtc2dcbmFkZE15U3ViIChFdmVyeSBpbnRlcnZhbCB0YWdnZXIpIHN0YXRlID1cbiAgY2FzZSBEaWN0LmdldCBpbnRlcnZhbCBzdGF0ZSBvZlxuICAgIE5vdGhpbmcgLT5cbiAgICAgIERpY3Quc2V0IGludGVydmFsIFt0YWdnZXJdIHN0YXRlXG5cbiAgICBKdXN0IHRhZ2dlcnMgLT5cbiAgICAgIERpY3Quc2V0IGludGVydmFsIChbdGFnZ2VyXSArKyB0YWdnZXJzKSBzdGF0ZVxuXG5cbnNwYXduSGVscCA6IFBsYXRmb3JtLlJvdXRlciBtc2cgRmxvYXQgLT4gQXJyYXkgRmxvYXQgLT4gUHJvY2Vzc2VzIC0+IFRhc2suVGFzayB4IFByb2Nlc3Nlc1xuc3Bhd25IZWxwIHJvdXRlciBpbnRlcnZhbHMgcHJvY2Vzc2VzID1cbiAgY2FzZSBBcnJheS5nZXQgMCBpbnRlcnZhbHMgb2ZcbiAgICBOb3RoaW5nIC0+XG4gICAgICBUYXNrLnN1Y2NlZWQgcHJvY2Vzc2VzXG5cbiAgICBKdXN0IGludGVydmFsIC0+XG4gICAgICBsZXRcbiAgICAgICAgc3Bhd25UaW1lciA9XG4gICAgICAgICAgUHJvY2Vzcy5zcGF3biAoc2V0SW50ZXJ2YWwgaW50ZXJ2YWwgKFBsYXRmb3JtLnNlbmRUb1NlbGYgcm91dGVyIGludGVydmFsKSlcblxuICAgICAgICByZXN0ID1cbiAgICAgICAgICBBcnJheS5zbGljZSAxIChBcnJheS5sZW5ndGggaW50ZXJ2YWxzKSBpbnRlcnZhbHNcblxuICAgICAgICBzcGF3blJlc3QgaWQgPVxuICAgICAgICAgIHNwYXduSGVscCByb3V0ZXIgcmVzdCAoRGljdC5zZXQgaW50ZXJ2YWwgaWQgcHJvY2Vzc2VzKVxuICAgICAgaW5cbiAgICAgICAgc3Bhd25UaW1lclxuICAgICAgICAgIHw+IFRhc2suYW5kVGhlbiBzcGF3blJlc3RcblxuXG5vblNlbGZNc2cgOiBQbGF0Zm9ybS5Sb3V0ZXIgbXNnIEZsb2F0IC0+IEZsb2F0IC0+IFN0YXRlIG1zZyAtPiBUYXNrIE5ldmVyIChTdGF0ZSBtc2cpXG5vblNlbGZNc2cgcm91dGVyIGludGVydmFsIHN0YXRlID1cbiAgY2FzZSBEaWN0LmdldCBpbnRlcnZhbCBzdGF0ZS50YWdnZXJzIG9mXG4gICAgTm90aGluZyAtPlxuICAgICAgVGFzay5zdWNjZWVkIHN0YXRlXG5cbiAgICBKdXN0IHRhZ2dlcnMgLT5cbiAgICAgIGxldFxuICAgICAgICB0ZWxsVGFnZ2VycyB0aW1lID1cbiAgICAgICAgICBUYXNrLnNlcXVlbmNlIChBcnJheS5tYXAgKFxcdGFnZ2VyIC0+IFBsYXRmb3JtLnNlbmRUb0FwcCByb3V0ZXIgKHRhZ2dlciB0aW1lKSkgdGFnZ2VycylcbiAgICAgIGluXG4gICAgICAgIG5vd1xuICAgICAgICAgIHw+IFRhc2suYW5kVGhlbiB0ZWxsVGFnZ2Vyc1xuICAgICAgICAgIHw+IFRhc2suYW5kVGhlbiAoXFxfIC0+IFRhc2suc3VjY2VlZCBzdGF0ZSlcblxuXG5zZXRJbnRlcnZhbCA6IEZsb2F0IC0+IFRhc2sgTmV2ZXIge30gLT4gVGFzayB4IE5ldmVyXG5zZXRJbnRlcnZhbCA9XG4gIEdyZW4uS2VybmVsLlRpbWUuc2V0SW50ZXJ2YWxcblxuXG5cbi0tIEZPUiBQQUNLQUdFIEFVVEhPUlNcblxuXG5cbnstfCAqKkludGVuZGVkIGZvciBwYWNrYWdlIGF1dGhvcnMuKipcblxuVGhlIGRvY3VtZW50YXRpb24gb2YgW2BoZXJlYF0oI2hlcmUpIGV4cGxhaW5zIHRoYXQgaXQgaGFzIGNlcnRhaW4gYWNjdXJhY3lcbmxpbWl0YXRpb25zIHRoYXQgYmxvY2sgb24gYWRkaW5nIG5ldyBBUElzIHRvIEphdmFTY3JpcHQuIFRoZSBgY3VzdG9tWm9uZWBcbmZ1bmN0aW9uIGlzIGEgc3RvcGdhcCB0aGF0IHRha2VzOlxuXG4xLiBBIGRlZmF1bHQgb2Zmc2V0IGluIG1pbnV0ZXMuIFNvIGBFdGMvR01ULTVgIGlzIGBjdXN0b21ab25lICgtNSAqIDYwKSBbXWBcbmFuZCBgRXRjL0dNVCs5YCBpcyBgY3VzdG9tWm9uZSAoOSAqIDYwKSBbXWAuXG4yLiBBIGxpc3Qgb2YgZXhjZXB0aW9ucyBjb250YWluaW5nIHRoZWlyIGBzdGFydGAgdGltZSBpbiBcIm1pbnV0ZXMgc2luY2UgdGhlIFVuaXhcbmVwb2NoXCIgYW5kIHRoZWlyIGBvZmZzZXRgIGluIFwibWludXRlcyBmcm9tIFVUQ1wiXG5cbkh1bWFuIHRpbWVzIHdpbGwgYmUgYmFzZWQgb24gdGhlIG5lYXJlc3QgYHN0YXJ0YCwgZmFsbGluZyBiYWNrIG9uIHRoZSBkZWZhdWx0XG5vZmZzZXQgaWYgdGhlIHRpbWUgaXMgb2xkZXIgdGhhbiBhbGwgb2YgdGhlIGV4Y2VwdGlvbnMuXG5cbldoZW4gcGFpcmVkIHdpdGggYGdldFpvbmVOYW1lYCwgdGhpcyBhbGxvd3MgeW91IHRvIGxvYWQgdGhlIHJlYWwgSUFOQSB0aW1lIHpvbmVcbmRhdGFiYXNlIGhvd2V2ZXIgeW91IHdhbnQ6IEhUVFAsIGNhY2hlLCBoYXJkY29kZSwgZXRjLlxuXG4qKk5vdGU6KiogSWYgeW91IHVzZSB0aGlzLCBwbGVhc2Ugc2hhcmUgeW91ciB3b3JrIGluIGFuIEdyZW4gY29tbXVuaXR5IGZvcnVtIVxuSSBhbSBzdXJlIG90aGVycyB3b3VsZCBsaWtlIHRvIGhlYXIgYWJvdXQgaXQsIGFuZCBtb3JlIGV4cGVyaWVuY2UgcmVwb3J0cyB3aWxsXG5oZWxwIG1lIGFuZCB0aGUgYW55IHBvdGVudGlhbCBUQzM5IHByb3Bvc2FsLlxuLX1cbmN1c3RvbVpvbmUgOiBJbnQgLT4gQXJyYXkgeyBzdGFydCA6IEludCwgb2Zmc2V0IDogSW50IH0gLT4gWm9uZVxuY3VzdG9tWm9uZSA9XG4gIFpvbmVcblxuXG57LXwgKipJbnRlbmRlZCBmb3IgcGFja2FnZSBhdXRob3JzLioqXG5cblVzZSBgSW50bC5EYXRlVGltZUZvcm1hdCgpLnJlc29sdmVkT3B0aW9ucygpLnRpbWVab25lYCB0byB0cnkgdG8gZ2V0IG5hbWVzXG5saWtlIGBFdXJvcGUvTW9zY293YCBvciBgQW1lcmljYS9IYXZhbmFgLiBGcm9tIHRoZXJlIHlvdSBjYW4gbG9vayBpdCB1cCBpbiBhbnlcbklBTkEgZGF0YSB5b3UgbG9hZGVkIHlvdXJzZWxmLlxuLX1cbmdldFpvbmVOYW1lIDogVGFzayB4IFpvbmVOYW1lXG5nZXRab25lTmFtZSA9XG4gIEdyZW4uS2VybmVsLlRpbWUuZ2V0Wm9uZU5hbWUge31cblxuXG57LXwgKipJbnRlbmRlZCBmb3IgcGFja2FnZSBhdXRob3JzLioqXG5cblRoZSBgZ2V0Wm9uZU5hbWVgIGZ1bmN0aW9uIHJlbGllcyBvbiBhIEphdmFTY3JpcHQgQVBJIHRoYXQgaXMgbm90IHN1cHBvcnRlZFxuaW4gYWxsIGJyb3dzZXJzIHlldCwgc28gaXQgY2FuIHJldHVybiB0aGUgZm9sbG93aW5nOlxuXG4gICAgLS0gaW4gbW9yZSByZWNlbnQgYnJvd3NlcnNcbiAgICBOYW1lIFwiRXVyb3BlL01vc2Nvd1wiXG4gICAgTmFtZSBcIkFtZXJpY2EvSGF2YW5hXCJcblxuICAgIC0tIGluIG9sZGVyIGJyb3dzZXJzXG4gICAgT2Zmc2V0IDE4MFxuICAgIE9mZnNldCAtMzAwXG5cblNvIGlmIHRoZSByZWFsIGluZm8gaXMgbm90IGF2YWlsYWJsZSwgaXQgd2lsbCB0ZWxsIHlvdSB0aGUgY3VycmVudCBVVEMgb2Zmc2V0XG5pbiBtaW51dGVzLCBqdXN0IGxpa2Ugd2hhdCBgaGVyZWAgdXNlcyB0byBtYWtlIHpvbmVzIGxpa2UgYGN1c3RvbVpvbmUgLTYwIFtdYC5cbi19XG50eXBlIFpvbmVOYW1lXG4gID0gTmFtZSBTdHJpbmdcbiAgfCBPZmZzZXQgSW50XG4iLAogICAgICAgICJlZmZlY3QgbW9kdWxlIEZpbGVTeXN0ZW0gd2hlcmUgeyBzdWJzY3JpcHRpb24gPSBGaWxlU3lzdGVtU3ViIH0gZXhwb3NpbmdcbiAgICAoIFBlcm1pc3Npb25cbiAgICAsIGluaXRpYWxpemVcbiAgICAtLSBFcnJvcnNcbiAgICAsIEVycm9yXG4gICAgLCBlcnJvckNvZGVcbiAgICAsIGVycm9yVG9TdHJpbmdcbiAgICAsIGVycm9ySXNQZXJtaXNzaW9uRGVuaWVkXG4gICAgLCBlcnJvcklzRmlsZUV4aXN0c1xuICAgICwgZXJyb3JJc0RpcmVjdG9yeUZvdW5kXG4gICAgLCBlcnJvcklzVG9vTWFueU9wZW5GaWxlc1xuICAgICwgZXJyb3JJc05vU3VjaEZpbGVPckRpcmVjdG9yeVxuICAgICwgZXJyb3JJc05vdEFEaXJlY3RvcnlcbiAgICAsIGVycm9ySXNEaXJlY3RvcnlOb3RFbXB0eVxuICAgICwgZXJyb3JJc05vdFBlcm1pdHRlZFxuICAgICwgZXJyb3JJc0xpbmtMb29wXG4gICAgLCBlcnJvcklzUGF0aFRvb0xvbmdcbiAgICAsIGVycm9ySXNJbnZhbGlkSW5wdXRcbiAgICAsIGVycm9ySXNJT1xuICAgIC0tIE1ldGFkYXRhXG4gICAgLCBNZXRhZGF0YVxuICAgICwgRW50aXR5VHlwZSguLilcbiAgICAsIG1ldGFkYXRhXG4gICAgLCBBY2Nlc3NQZXJtaXNzaW9uKC4uKVxuICAgICwgY2hlY2tBY2Nlc3NcbiAgICAsIGNoYW5nZUFjY2Vzc1xuICAgICwgYWNjZXNzUGVybWlzc2lvbnNUb0ludFxuICAgICwgY2hhbmdlT3duZXJcbiAgICAsIGNoYW5nZVRpbWVzXG4gICAgLCBtb3ZlXG4gICAgLCByZWFsUGF0aFxuICAgIC0tIEZpbGVzXG4gICAgLCBhcHBlbmRUb0ZpbGVcbiAgICAsIGNvcHlGaWxlXG4gICAgLCByZWFkRmlsZVxuICAgICwgd3JpdGVGaWxlXG4gICAgLCB0cnVuY2F0ZUZpbGVcbiAgICAsIHJlbW92ZVxuICAgIC0tIERpcmVjdG9yaWVzXG4gICAgLCBsaXN0RGlyZWN0b3J5XG4gICAgLCBtYWtlRGlyZWN0b3J5XG4gICAgLCBtYWtlVGVtcERpcmVjdG9yeVxuICAgIC0tIExpbmtzXG4gICAgLCBoYXJkTGlua1xuICAgICwgc29mdExpbmtcbiAgICAsIHJlYWRMaW5rXG4gICAgLCB1bmxpbmtcbiAgICAtLSBXYXRjaFxuICAgICwgV2F0Y2hFdmVudCguLilcbiAgICAsIHdhdGNoXG4gICAgLCB3YXRjaFJlY3Vyc2l2ZVxuICAgIC0tXG4gICAgLCBob21lRGlyZWN0b3J5XG4gICAgLCBjdXJyZW50V29ya2luZ0RpcmVjdG9yeVxuICAgICwgdG1wRGlyZWN0b3J5XG4gICAgLCBkZXZOdWxsXG4gICAgKVxuXG5cbnstfCBUaGlzIG1vZHVsZSBwcm92aWRlcyBhY2Nlc3MgdG8gdGhlIGZpbGUgc3lzdGVtLiBJdCBhbGxvd3MgeW91IHRvIHJlYWQgYW5kIHdyaXRlIGZpbGVzLCBjcmVhdGUgZGlyZWN0b3JpZXMgYW5kIGxpbmtzIGV0Yy5cblxuQGRvY3MgUGVybWlzc2lvbiwgaW5pdGlhbGl6ZVxuXG4jIyBFcnJvcnNcblxuQGRvY3MgRXJyb3IsIGVycm9yQ29kZSwgZXJyb3JUb1N0cmluZ1xuQGRvY3MgZXJyb3JJc1Blcm1pc3Npb25EZW5pZWQsIGVycm9ySXNGaWxlRXhpc3RzLCBlcnJvcklzRGlyZWN0b3J5Rm91bmQsIGVycm9ySXNUb29NYW55T3BlbkZpbGVzLCBlcnJvcklzTm9TdWNoRmlsZU9yRGlyZWN0b3J5LCBlcnJvcklzTm90QURpcmVjdG9yeSwgZXJyb3JJc0RpcmVjdG9yeU5vdEVtcHR5LCBlcnJvcklzTm90UGVybWl0dGVkLCBlcnJvcklzTGlua0xvb3AsIGVycm9ySXNQYXRoVG9vTG9uZywgZXJyb3JJc0ludmFsaWRJbnB1dCwgZXJyb3JJc0lPXG5cbiMjIE1ldGFkYXRhXG5cbkBkb2NzIE1ldGFkYXRhLCBFbnRpdHlUeXBlLCBtZXRhZGF0YSwgQWNjZXNzUGVybWlzc2lvbiwgY2hlY2tBY2Nlc3MsIGNoYW5nZUFjY2VzcywgYWNjZXNzUGVybWlzc2lvbnNUb0ludCwgY2hhbmdlT3duZXIsIGNoYW5nZVRpbWVzLCBtb3ZlLCByZWFsUGF0aFxuXG4jIyBGaWxlc1xuXG5AZG9jcyBjb3B5RmlsZSwgYXBwZW5kVG9GaWxlLCByZWFkRmlsZSwgd3JpdGVGaWxlLCB0cnVuY2F0ZUZpbGUsIHJlbW92ZVxuXG4jIyBEaXJlY3Rvcmllc1xuXG5AZG9jcyBsaXN0RGlyZWN0b3J5LCBtYWtlRGlyZWN0b3J5LCBtYWtlVGVtcERpcmVjdG9yeSBcblxuIyMgTGlua3NcblxuQGRvY3MgaGFyZExpbmssIHNvZnRMaW5rLCByZWFkTGluaywgdW5saW5rXG5cbiMjIFdhdGNoIGZvciBjaGFuZ2VzXG5cbkBkb2NzIFdhdGNoRXZlbnQsIHdhdGNoLCB3YXRjaFJlY3Vyc2l2ZVxuXG4jIyBTcGVjaWFsIHBhdGhzXG5cbkBkb2NzIGhvbWVEaXJlY3RvcnksIGN1cnJlbnRXb3JraW5nRGlyZWN0b3J5LCB0bXBEaXJlY3RvcnksIGRldk51bGxcbi19XG5cblxuaW1wb3J0IEdyZW4uS2VybmVsLkZpbGVTeXN0ZW1cbmltcG9ydCBHcmVuLktlcm5lbC5GaWxlUGF0aFxuaW1wb3J0IEJ5dGVzIGV4cG9zaW5nIChCeXRlcylcbmltcG9ydCBEaWN0IGV4cG9zaW5nIChEaWN0KVxuaW1wb3J0IFRhc2sgZXhwb3NpbmcgKFRhc2spXG5pbXBvcnQgRmlsZVN5c3RlbS5QYXRoIGV4cG9zaW5nIChQYXRoKVxuaW1wb3J0IEluaXRcbmltcG9ydCBJbnRlcm5hbC5Jbml0XG5pbXBvcnQgVGltZVxuaW1wb3J0IFByb2Nlc3NcblxuXG57LXwgVGhpcyB2YWx1ZSByZXByZXNlbnRzIHRoZSBwZXJtaXNzaW9uIHRvIHBlcmZvcm0gZmlsZSBzeXN0ZW0gb3BlcmF0aW9ucy5cblxuT25seSBjb2RlIHlvdSB0cnVzdCBzaG91bGQgaGF2ZSBhY2Nlc3MgdG8gdGhpcyB2YWx1ZS5cbi19XG50eXBlIFBlcm1pc3Npb25cbiAgICA9IFBlcm1pc3Npb25cblxuXG57LXwgSW5pdGlhbGl6ZSB0aGUgYEZpbGVTeXN0ZW1gIHN1YnN5c3RlbSwgd2hpY2ggZ2FpbnMgeW91IHRoZSBwZXJtaXNzaW9uIHRvIHBlcmZvcm1cbmZpbGUgc3lzdGVtIG9wZXJhdGlvbnMuXG4tfVxuaW5pdGlhbGl6ZSA6IEluaXQuVGFzayBQZXJtaXNzaW9uXG5pbml0aWFsaXplID1cbiAgICBUYXNrLnN1Y2NlZWQgUGVybWlzc2lvblxuICAgICAgICB8PiBJbnRlcm5hbC5Jbml0LlRhc2tcblxuXG4tLSBFUlJPUlNcblxuXG57LXwgUmVwcmVzZW50cyBhbiBlcnJvciB0aGF0IG9jY3VyZWQgd2hlbiB3b3JraW5nIHdpdGggdGhlIGZpbGUgc3lzdGVtLlxuXG5UaGVyZSBhcmUgbWFueSBkaWZmZXJlbnQga2luZHMgb2YgZXJyb3IgZGVwZW5kaW5nIG9uIHdoaWNoIG9wZXJhdGlvbiB5b3UncmUgcGVyZm9ybWluZyBhbmQgd2hpY2hcbm9wZXJhdGluZyBzeXN0ZW0geW91J3JlIHBlcmZvcm1pbmcgaXQgb24uIFRvIGZpZ3VyZSBvdXQgd2hpY2ggZXJyb3IgaXQgaXMsIHlvdSdsbCBuZWVkXG50byB1c2Ugb25lIG9mIHRoZSBoZWxwZXIgZnVuY3Rpb25zIGJlbG93LCBvciBjaGVjayB0aGUgc3BlY2lmaWMgZXJyb3IgY29kZS5cbi19XG50eXBlIEVycm9yXG4gICAgPSBFcnJvciBTdHJpbmcgU3RyaW5nXG5cblxuey18IEEgc3RyaW5nIHRoYXQgaWRlbnRpZmllcyBhIHNwZWNpZmljIGtpbmQgb2YgZXJyb3IuIFRoZXJlIGNhbiBiZSBzZXZlcmFsIGVycm9yIGNvZGVzIGZvciB0aGVcbnNhbWUga2luZCBvZiBlcnJvciwgZGVwZW5kaW5nIG9uIHRoZSBvcGVyYXRpbmcgc3lzdGVtIHRoYXQgaXMgaW4gdXNlLlxuLX1cbmVycm9yQ29kZSA6IEVycm9yIC0+IFN0cmluZ1xuZXJyb3JDb2RlIChFcnJvciBjb2RlIF8pID1cbiAgICBjb2RlXG5cblxuey18IFJldHVybnMgYSBodW1hbiByZWFkYWJsZSBkZXNjcmlwdGlvbiBvZiB0aGUgZXJyb3IuXG4tfVxuZXJyb3JUb1N0cmluZyA6IEVycm9yIC0+IFN0cmluZ1xuZXJyb3JUb1N0cmluZyAoRXJyb3IgXyBtZXNzYWdlKSA9XG4gICAgbWVzc2FnZVxuXG5cbnstfCBJZiBgVHJ1ZWAsIHRoZSBlcnJvciBvY2N1cmVkIGJlY2F1c2UgeW91IGRvbid0IGhhdmUgdGhlIGNvcnJlY3QgYWNjZXNzIHBlcm1pc3Npb24gdG8gcGVyZm9ybVxudGhlIG9wZXJhdGlvbi5cbi19XG5lcnJvcklzUGVybWlzc2lvbkRlbmllZCA6IEVycm9yIC0+IEJvb2xcbmVycm9ySXNQZXJtaXNzaW9uRGVuaWVkIChFcnJvciBjb2RlIF8pID1cbiAgICBjb2RlID09IFwiRUFDQ0VTXCJcblxuXG57LXwgSWYgYFRydWVgLCBhIGZpbGUgZXhpc3RzIHdoZW4gaXQgd2FzIGV4cGVjdGVkIG5vdCB0by5cbi19XG5lcnJvcklzRmlsZUV4aXN0cyA6IEVycm9yIC0+IEJvb2xcbmVycm9ySXNGaWxlRXhpc3RzIChFcnJvciBjb2RlIF8pID1cbiAgICBjb2RlID09IFwiRUVYSVNUXCJcblxuXG57LXwgSWYgYFRydWVgLCBhIGZpbGUgb3BlcmF0aW9uIHdhcyBhdHRlbXB0ZWQgb24gYSBkaXJlY3RvcnkuXG4tfVxuZXJyb3JJc0RpcmVjdG9yeUZvdW5kIDogRXJyb3IgLT4gQm9vbFxuZXJyb3JJc0RpcmVjdG9yeUZvdW5kIChFcnJvciBjb2RlIF8pID1cbiAgICBjb2RlID09IFwiRUlTRElSXCJcblxuXG57LXwgSWYgYFRydWVgLCB0aGUgYXBwbGljYXRpb24gaGFzIHRvbyBtYW55IG9wZW4gZmlsZXMuXG4tfVxuZXJyb3JJc1Rvb01hbnlPcGVuRmlsZXMgOiBFcnJvciAtPiBCb29sXG5lcnJvcklzVG9vTWFueU9wZW5GaWxlcyAoRXJyb3IgY29kZSBfKSA9XG4gICAgY29kZSA9PSBcIkVNRklMRVwiXG5cblxuey18IElmIGBUcnVlYCwgdGhlIGNvZGUgd2FzIHBhc3NlZCBhIFtQYXRoXShGaWxlU3lzdGVtLlBhdGgjUGF0aCkgd2hpY2ggcG9pbnRzIHRvIGEgZmlsZSBvciBkaXJlY3RvcnlcbnRoYXQgZG9lc24ndCBleGlzdC5cbi19XG5lcnJvcklzTm9TdWNoRmlsZU9yRGlyZWN0b3J5IDogRXJyb3IgLT4gQm9vbFxuZXJyb3JJc05vU3VjaEZpbGVPckRpcmVjdG9yeSAoRXJyb3IgY29kZSBfKSA9XG4gICAgY29kZSA9PSBcIkVOT0VOVFwiXG5cblxuey18IElmIGBUcnVlYCwgYSBkaXJlY3Rvcnkgd2FzIGV4cGVjdGVkIGJ1dCBpdCBmb3VuZCBhIGZpbGUgb3Igc29tZSBvdGhlciBlbnRpdHkuXG4tfVxuZXJyb3JJc05vdEFEaXJlY3RvcnkgOiBFcnJvciAtPiBCb29sXG5lcnJvcklzTm90QURpcmVjdG9yeSAoRXJyb3IgY29kZSBfKSA9XG4gICAgY29kZSA9PSBcIkVOT1RESVJcIlxuXG5cbnstfCBJZiBgVHJ1ZWAsIHRoZSBvcGVyYXRpb24gZXhwZWN0ZWQgYW4gZW1wdHkgZGlyZWN0b3J5LCBidXQgdGhlIGRpcmVjdG9yeSBpcyBub3QgZW1wdHkuXG4tfVxuZXJyb3JJc0RpcmVjdG9yeU5vdEVtcHR5IDogRXJyb3IgLT4gQm9vbFxuZXJyb3JJc0RpcmVjdG9yeU5vdEVtcHR5IChFcnJvciBjb2RlIF8pID1cbiAgICBjb2RlID09IFwiRU5PVEVNUFRZXCJcblxuXG57LXwgSWYgYFRydWVgLCB0aGUgb3BlcmF0aW9uIHdhcyByZWplY3RlZCBiZWNhdXNlIG9mIG1pc3NpbmcgcHJpdmlsZWdlcy5cbi19XG5lcnJvcklzTm90UGVybWl0dGVkIDogRXJyb3IgLT4gQm9vbFxuZXJyb3JJc05vdFBlcm1pdHRlZCAoRXJyb3IgY29kZSBfKSA9XG4gICAgY29kZSA9PSBcIkVQRVJNXCJcblxuXG57LXwgSWYgYFRydWVgLCB3ZSBzZWVtIHRvIGJlIHN0dWNrIGluIGEgbG9vcCBmb2xsb3dpbmcgbGluayBhZnRlciBsaW5rIGFmdGVyLi4uXG4tfVxuZXJyb3JJc0xpbmtMb29wIDogRXJyb3IgLT4gQm9vbFxuZXJyb3JJc0xpbmtMb29wIChFcnJvciBjb2RlIF8pID1cbiAgICBjb2RlID09IFwiRUxPT1BcIlxuXG5cbnstfCBJZiBgVHJ1ZWAsIHRoZSBbUGF0aF0oRmlsZVN5c3RlbS5QYXRoI1BhdGgpIGlzIHRvbyBsb25nLlxuLX1cbmVycm9ySXNQYXRoVG9vTG9uZyA6IEVycm9yIC0+IEJvb2xcbmVycm9ySXNQYXRoVG9vTG9uZyAoRXJyb3IgY29kZSBfKSA9XG4gICAgY29kZSA9PSBcIkVOQU1FVE9PTE9OR1wiXG5cblxuey18IElmIGBUcnVlYCwgdGhlIGFyZ3VtZW50cyBwYXNzZWQgdG8gdGhlIGZ1bmN0aW9uIGlzIGludmFsaWQgc29tZWhvdy5cbi19XG5lcnJvcklzSW52YWxpZElucHV0IDogRXJyb3IgLT4gQm9vbFxuZXJyb3JJc0ludmFsaWRJbnB1dCAoRXJyb3IgY29kZSBfKSA9XG4gICAgY29kZSA9PSBcIkVJTlZBTFwiXG5cblxuey18IElmIGBUcnVlYCwgdGhlIG9wZXJhdGlvbiBmYWlsZWQgZHVlIHRvIGFuIElPIGVycm9yLiBUaGlzIGNvdWxkIGJlIHRoYXQgdGhlIGRpc2sgaXNcbmJ1c3ksIG9yIGV2ZW4gY29ycnVwdC5cbi19XG5lcnJvcklzSU8gOiBFcnJvciAtPiBCb29sXG5lcnJvcklzSU8gKEVycm9yIGNvZGUgXykgPVxuICAgIGNvZGUgPT0gXCJFSU9cIlxuXG5cbi0tIE1FVEFEQVRBXG5cblxuey18IFJlcHJlc2VudHMgZXh0cmEgaW5mb3JtYXRpb24gYWJvdXQgYW4gZW50aXR5IGluIHRoZSBmaWxlIHN5c3RlbS5cbi19XG50eXBlIGFsaWFzIE1ldGFkYXRhID1cbiAgICB7IGVudGl0eVR5cGUgOiBFbnRpdHlUeXBlXG4gICAgLCBkZXZpY2VJRCA6IEludFxuICAgICwgdXNlcklEIDogSW50XG4gICAgLCBncm91cElEIDogSW50XG4gICAgLCBieXRlU2l6ZSA6IEludFxuICAgICwgYmxvY2tTaXplIDogSW50XG4gICAgLCBibG9ja3MgOiBJbnRcbiAgICAsIGxhc3RBY2Nlc3NlZCA6IFRpbWUuUG9zaXhcbiAgICAsIGxhc3RNb2RpZmllZCA6IFRpbWUuUG9zaXhcbiAgICAsIGxhc3RDaGFuZ2VkIDogVGltZS5Qb3NpeFxuICAgICwgY3JlYXRlZCA6IFRpbWUuUG9zaXhcbiAgICB9XG5cblxuey18IFRoZSB0eXBlIG9mIGFuIGVudGl0eSBpbiB0aGUgZmlsZSBzeXN0ZW0uXG4tfVxudHlwZSBFbnRpdHlUeXBlXG4gICAgPSBGaWxlXG4gICAgfCBEaXJlY3RvcnlcbiAgICB8IFNvY2tldFxuICAgIHwgU3ltbGlua1xuICAgIHwgRGV2aWNlXG4gICAgfCBQaXBlXG5cblxuey18IFJldHVybiBtZXRhZGF0YSBmb3IgdGhlIGVudGl0eSByZXByZXNlbnRlZCBieSBbUGF0aF0oRmlsZVN5c3RlbS5QYXRoI1BhdGgpLlxuXG5JZiBgcmVzb2x2ZUxpbmtgIGlzIGBGYWxzZWAsIHlvdSB3aWxsIHJlY2VpdmUgbWV0YWRhdGEgZm9yIHRoZSBsaW5rIGl0c2VsZiwgbm90IHRoZSBlbnRpdHlcbnBvaW50ZWQgYXQgYnkgdGhlIGxpbmsuXG4tfVxubWV0YWRhdGEgOiBQZXJtaXNzaW9uIC0+IHsgcmVzb2x2ZUxpbmsgOiBCb29sIH0gLT4gUGF0aCAtPiBUYXNrIEVycm9yIE1ldGFkYXRhXG5tZXRhZGF0YSBfIHsgcmVzb2x2ZUxpbmsgfSBwYXRoID1cbiAgICBpZiByZXNvbHZlTGluayB0aGVuXG4gICAgICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0uc3RhdCBwYXRoXG5cbiAgICBlbHNlXG4gICAgICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0ubHN0YXQgcGF0aFxuXG5cbnstfCBSZXByZXNlbnRzIHRoZSBwZXJtaXNzaW9uIHRvIGFjY2VzcyBhbiBlbnRpdHkgZm9yIGEgc3BlY2lmaWMgb3BlcmF0aW9uLlxuXG5Gb3IgZXhhbXBsZTogaWYgeW91LCBvciB5b3VyIGdyb3VwLCBkb2Vzbid0IGhhdmUgdGhlIGBSZWFkYCBwZXJtaXNzaW9uIGZvciBhIGZpbGUsXG55b3UncmUgbm90IGFsbG93ZWQgdG8gcmVhZCBmcm9tIGl0LlxuLX1cbnR5cGUgQWNjZXNzUGVybWlzc2lvblxuICAgID0gUmVhZFxuICAgIHwgV3JpdGVcbiAgICB8IEV4ZWN1dGVcblxuXG57LXwgQ2hlY2sgaWYgdGhlIHVzZXIgcnVubmluZyB0aGlzIGFwcGxpY2F0aW9uIGhhcyB0aGUgZ2l2ZW4gYWNjZXNzIHBlcm1pc3Npb25zIGZvciB0aGVcbmVudGl0eSByZXByZXNlbnRlZCBieSBbUGF0aF0oRmlsZVN5c3RlbS5QYXRoI1BhdGgpLlxuXG5QYXNzaW5nIGFuIGVtcHR5IGBBcnJheWAgd2lsbCBjaGVjayB0aGF0IHRoZSBlbnRpdHkgZXhpc3RzLlxuLX1cbmNoZWNrQWNjZXNzIDogUGVybWlzc2lvbiAtPiBBcnJheSBBY2Nlc3NQZXJtaXNzaW9uIC0+IFBhdGggLT4gVGFzayBFcnJvciBQYXRoXG5jaGVja0FjY2VzcyBfIHBlcm1pc3Npb25zIHBhdGggPVxuICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0uYWNjZXNzIHBlcm1pc3Npb25zIHBhdGhcblxuXG57LXwgQ2hhbmdlIHRoZSBhY2Nlc3MgcGVybWlzc2lvbnMgZm9yIHRoZSBlbnRpdHkncyBvd25lciwgZ3JvdXAgYW5kIGV2ZXJ5b25lIGVsc2UuXG4tfVxuY2hhbmdlQWNjZXNzIFxuICAgIDogUGVybWlzc2lvblxuICAgIC0+IHsgb3duZXIgOiBBcnJheSBBY2Nlc3NQZXJtaXNzaW9uIFxuICAgICAgICwgZ3JvdXAgOiBBcnJheSBBY2Nlc3NQZXJtaXNzaW9uXG4gICAgICAgLCBvdGhlcnMgOiBBcnJheSBBY2Nlc3NQZXJtaXNzaW9uXG4gICAgICAgfVxuICAgIC0+IFBhdGhcbiAgICAtPiBUYXNrIEVycm9yIFBhdGhcbmNoYW5nZUFjY2VzcyBfIHBlcm1pc3Npb25zIHBhdGggPVxuICAgIGxldFxuICAgICAgICBtb2RlID1cbiAgICAgICAgICAgIChTdHJpbmcuZnJvbUludCA8fCBhY2Nlc3NQZXJtaXNzaW9uc1RvSW50IHBlcm1pc3Npb25zLm93bmVyKVxuICAgICAgICAgICAgKysgKFN0cmluZy5mcm9tSW50IDx8IGFjY2Vzc1Blcm1pc3Npb25zVG9JbnQgcGVybWlzc2lvbnMuZ3JvdXApXG4gICAgICAgICAgICArKyAoU3RyaW5nLmZyb21JbnQgPHwgYWNjZXNzUGVybWlzc2lvbnNUb0ludCBwZXJtaXNzaW9ucy5vdGhlcnMpXG4gICAgaW5cbiAgICBHcmVuLktlcm5lbC5GaWxlU3lzdGVtLmNobW9kIG1vZGUgcGF0aFxuXG5cbnstfCBUaGUgaW50ZWdlciByZXByZXNlbnRhdGlvbiBvZiBhIHNldCBvZiBhY2Nlc3MgcGVybWlzc2lvbnMgaW4gYSBwb3NpeCBzeXN0ZW0uXG5cbiAgICBhY2Nlc3NQZXJtaXNzaW9uc1RvSW50IFsgUmVhZCwgV3JpdGUgXSA9PSA2XG4tfVxuYWNjZXNzUGVybWlzc2lvbnNUb0ludCA6IEFycmF5IEFjY2Vzc1Blcm1pc3Npb24gLT4gSW50XG5hY2Nlc3NQZXJtaXNzaW9uc1RvSW50IHZhbHVlcyA9XG4gICAgbGV0XG4gICAgICAgIG51bWJlckZvciBudW0gYSA9XG4gICAgICAgICAgICBpZiBBcnJheS5tZW1iZXIgYSB2YWx1ZXMgdGhlblxuICAgICAgICAgICAgICAgIG51bVxuXG4gICAgICAgICAgICBlbHNlXG4gICAgICAgICAgICAgICAgMFxuICAgIGluXG4gICAgbnVtYmVyRm9yIDQgUmVhZCArIG51bWJlckZvciAyIFdyaXRlICsgbnVtYmVyRm9yIDEgRXhlY3V0ZVxuXG5cbnstfCBDaGFuZ2UgdGhlIHVzZXIgYW5kIGdyb3VwIHRoYXQgb3ducyBhIGZpbGUuXG5cbllvdSdsbCBuZWVkIHRoZSBJRCBvZiB0aGUgb3duZXIgYW5kIGdyb3VwIHRvIHBlcmZvcm0gdGhpcyBvcGVyYXRpb24uXG5cbklmIGByZXNvbHZlTGlua2AgaXMgYEZhbHNlYCwgeW91J3JlIGNoYW5naW5nIHRoZSBvd25lciBvZiB0aGUgbGluayBpdHNlbGYsXG5ub3QgdGhlIGVudGl0eSBpdCBwb2ludHMgdG8uXG4tfVxuY2hhbmdlT3duZXIgOiBQZXJtaXNzaW9uIC0+IHsgdXNlcklEIDogSW50LCBncm91cElEIDogSW50LCByZXNvbHZlTGluayA6IEJvb2wgfSAtPiBQYXRoIC0+IFRhc2sgRXJyb3IgUGF0aFxuY2hhbmdlT3duZXIgXyBvcHRpb25zIHBhdGggPVxuICAgIGlmIG9wdGlvbnMucmVzb2x2ZUxpbmsgdGhlblxuICAgICAgICBHcmVuLktlcm5lbC5GaWxlU3lzdGVtLmNob3duIG9wdGlvbnMgcGF0aFxuXG4gICAgZWxzZVxuICAgICAgICBHcmVuLktlcm5lbC5GaWxlU3lzdGVtLmxjaG93biBvcHRpb25zIHBhdGhcblxuXG57LXwgQ2hhbmdlIHRoZSByZWdpc3RlcmVkIHRpbWUgKGRvd24gdG8gdGhlIHNlY29uZCkgYW4gZW50aXR5IHdhcyBhY2Nlc3NlZCBhbmQgbW9kaWZpZWQuXG5cbklmIGByZXNvbHZlTGlua2AgaXMgYEZhbHNlYCwgeW91J3JlIGNoYW5naW5nIHRoZSBsYXN0IGFjY2VzcyBhbmQgbW9kaWZpY2F0aW9uIHRpbWUgb2YgdGhlIGxpbmsgaXRzZWxmLFxubm90IHRoZSBlbnRpdHkgaXQgcG9pbnRzIHRvLlxuLX1cbmNoYW5nZVRpbWVzIDogUGVybWlzc2lvbiAtPiB7IGxhc3RBY2Nlc3NlZCA6IFRpbWUuUG9zaXgsIGxhc3RNb2RpZmllZCA6IFRpbWUuUG9zaXgsIHJlc29sdmVMaW5rIDogQm9vbCB9IC0+IFBhdGggLT4gVGFzayBFcnJvciBQYXRoXG5jaGFuZ2VUaW1lcyBfIHsgbGFzdEFjY2Vzc2VkLCBsYXN0TW9kaWZpZWQsIHJlc29sdmVMaW5rIH0gcGF0aCA9XG4gICAgbGV0XG4gICAgICAgIGxhc3RBY2Nlc3NlZFNlY29uZHMgPSBcbiAgICAgICAgICAgIFRpbWUucG9zaXhUb01pbGxpcyBsYXN0QWNjZXNzZWQgLy8gMTAwMFxuICAgICAgICBcbiAgICAgICAgbGFzdE1vZGlmaWVkU2Vjb25kcyA9IFxuICAgICAgICAgICAgVGltZS5wb3NpeFRvTWlsbGlzIGxhc3RNb2RpZmllZCAvLyAxMDAwXG4gICAgaW5cbiAgICBpZiByZXNvbHZlTGluayB0aGVuXG4gICAgICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0udXRpbWVzIGxhc3RBY2Nlc3NlZFNlY29uZHMgbGFzdE1vZGlmaWVkU2Vjb25kcyBwYXRoXG5cbiAgICBlbHNlXG4gICAgICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0ubHV0aW1lcyBsYXN0QWNjZXNzZWRTZWNvbmRzIGxhc3RNb2RpZmllZFNlY29uZHMgcGF0aFxuXG5cbnstfCBNb3ZlIHRoZSBlbnRpdHkgcmVwcmVzZW50ZWQgYnkgdGhlIGxhc3QgW1BhdGhdKEZpbGVTeXN0ZW0uUGF0aCNQYXRoKSwgdG8gdGhlIGxvY2F0aW9uXG5yZXByZXNlbnRlZCBieSB0aGUgZmlyc3QgW1BhdGhdKEZpbGVTeXN0ZW0uUGF0aCNQYXRoKS4gVGhpcyBjYW4gYWxzbyBiZSB1c2VkIHRvIHJlbmFtZSBhblxuZW50aXR5LlxuLX1cbm1vdmUgOiBQZXJtaXNzaW9uIC0+IFBhdGggLT4gUGF0aCAtPiBUYXNrIEVycm9yIFBhdGhcbm1vdmUgXyBuZXcgb2xkID1cbiAgICBHcmVuLktlcm5lbC5GaWxlU3lzdGVtLnJlbmFtZSBvbGQgbmV3XG5cblxuey18IElmIHlvdSBoYXZlIGEgW1BhdGhdKEZpbGVTeXN0ZW0uUGF0aCNQYXRoKSB0aGF0IGlzIHJlbGF0aXZlIHRvIHRoZSBjdXJyZW50IGRpcmVjdG9yeSxcbm9yIHBvaW50cyBhdCBhIGxpbmssIHlvdSBjYW4gdXNlIHRoaXMgZmluZCB0aGUgdHJ1ZSBbUGF0aF0oRmlsZVN5c3RlbS5QYXRoI1BhdGgpIG9mIHRoZVxuZW50aXR5LlxuLX1cbnJlYWxQYXRoIDogUGVybWlzc2lvbiAtPiBQYXRoIC0+IFRhc2sgRXJyb3IgUGF0aFxucmVhbFBhdGggXyBwYXRoID1cbiAgICBHcmVuLktlcm5lbC5GaWxlU3lzdGVtLnJlYWxwYXRoIHBhdGhcblxuXG4tLSBGSUxFU1xuXG5cbnstfCBBZGQgYEJ5dGVzYCB0byB0aGUgZW5kIG9mIGEgZmlsZS5cbi19XG5hcHBlbmRUb0ZpbGUgOiBQZXJtaXNzaW9uIC0+IEJ5dGVzIC0+IFBhdGggLT4gVGFzayBFcnJvciBQYXRoXG5hcHBlbmRUb0ZpbGUgXyBieXRlcyBwYXRoID1cbiAgICBHcmVuLktlcm5lbC5GaWxlU3lzdGVtLmFwcGVuZEZpbGUgYnl0ZXMgcGF0aFxuXG5cbnstfCBDb3B5IHRoZSBmaWxlIHJlcHJlc2VudGVkIGJ5IHRoZSBsYXN0IFtQYXRoXShGaWxlU3lzdGVtLlBhdGgjUGF0aCksIHRvIHRoZSBsb2NhdGlvblxucmVwcmVzZW50ZWQgYnkgdGhlIGZpcnN0IFtQYXRoXShGaWxlU3lzdGVtLlBhdGgjUGF0aCkuXG4tfVxuY29weUZpbGUgOiBQZXJtaXNzaW9uIC0+IFBhdGggLT4gUGF0aCAtPiBUYXNrIEVycm9yIFBhdGhcbmNvcHlGaWxlIF8gZGVzdCBzcmMgPVxuICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0uY29weUZpbGUgc3JjIGRlc3RcblxuXG57LXwgUmVhZCB0aGUgZW50aXJlIGNvbnRlbnRzIG9mIGEgZmlsZS5cbi19XG5yZWFkRmlsZSA6IFBlcm1pc3Npb24gLT4gUGF0aCAtPiBUYXNrIEVycm9yIEJ5dGVzXG5yZWFkRmlsZSBfIHBhdGggPVxuICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0ucmVhZEZpbGUgcGF0aFxuXG5cbnstfCBXcml0ZSB0aGUgZ2l2ZW4gYEJ5dGVzYCBpbnRvIGEgZmlsZS4gVGhlIGZpbGUgd2lsbCBiZSBjcmVhdGVkIGlmIGl0IGRvZXNuJ3QgZXhpc3QsXG5hbmQgb3ZlcndyaXR0ZW4gaWYgaXQgZG9lcy5cbi19XG53cml0ZUZpbGUgOiBQZXJtaXNzaW9uIC0+IEJ5dGVzIC0+IFBhdGggLT4gVGFzayBFcnJvciBQYXRoXG53cml0ZUZpbGUgXyBieXRlcyBwYXRoID1cbiAgICBHcmVuLktlcm5lbC5GaWxlU3lzdGVtLndyaXRlRmlsZSBieXRlcyBwYXRoXG5cblxuey18IE1ha2Ugc3VyZSB0aGUgZ2l2ZW4gZmlsZSBpcyBvZiBhIHNwZWNpZmljIGxlbmd0aC4gSWYgdGhlIGZpbGUgaXMgc21hbGxlciB0aGFuXG50aGUgZ2l2ZW4gbGVuZ3RoLCB6ZXJvZXMgaXMgYWRkZWQgdG8gdGhlIGZpbGUgdW50aWwgaXQgaXMgdGhlIGNvcnJlY3QgbGVuZ3RoLiBJZiB0aGUgZmlsZVxuaXMgbGFyZ2VyIHRoYW4gdGhlIGdpdmVuIGxlbmd0aCwgdGhlIGV4Y2VzcyBieXRlcyBhcmUgcmVtb3ZlZC5cbi19XG50cnVuY2F0ZUZpbGUgOiBQZXJtaXNzaW9uIC0+IEludCAtPiBQYXRoIC0+IFRhc2sgRXJyb3IgUGF0aFxudHJ1bmNhdGVGaWxlIF8gbGVuZ3RoIHBhdGggPVxuICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0udHJ1bmNhdGUgbGVuZ3RoIHBhdGhcblxuXG57LXwgUmVtb3ZlIHRoZSBmaWxlIG9yIGRpcmVjdG9yeSBhdCB0aGUgZ2l2ZW4gcGF0aC5cblxuKiBgcmVjdXJzaXZlYCB3aWxsIGRlbGV0ZSBldmVyeXRoaW5nIGluc2lkZSBhIGRpcmVjdG9yeS5cbiogYGlnbm9yZUVycm9yc2Agd2lsbC4uLiBpZ25vcmUgYW55IGVycm9ycyByZWxhdGVkIHRvIGEgcmVtb3ZlIG9wZXJhdGlvbi5cbi19XG5yZW1vdmUgOiBQZXJtaXNzaW9uIC0+IHsgcmVjdXJzaXZlIDogQm9vbCwgaWdub3JlRXJyb3JzIDogQm9vbCB9IC0+IFBhdGggLT4gVGFzayBFcnJvciBQYXRoXG5yZW1vdmUgXyBvcHRpb25zIHBhdGggPVxuICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0ucmVtb3ZlIG9wdGlvbnMgcGF0aFxuXG5cbi0tIERJUkVDVE9SSUVTXG5cblxuey18IExpc3QgdGhlIGNvbnRlbnRzIG9mIGEgZGlyZWN0b3J5LiBUaGUgcmV0dXJuZWQgW1BhdGhzXShGaWxlU3lzdGVtLlBhdGgjUGF0aCkgYXJlIHJlbGF0aXZlIHRvXG50aGUgZGlyZWN0b3J5IGJlaW5nIGxpc3RlZC5cbi19XG5saXN0RGlyZWN0b3J5IDogUGVybWlzc2lvbiAtPiBQYXRoIC0+IFRhc2sgRXJyb3IgKEFycmF5IHsgcGF0aCA6IFBhdGgsIGVudGl0eVR5cGUgOiBFbnRpdHlUeXBlIH0pXG5saXN0RGlyZWN0b3J5IF8gcGF0aCA9XG4gICAgR3Jlbi5LZXJuZWwuRmlsZVN5c3RlbS5saXN0RGlyZWN0b3J5IHBhdGhcblxuXG57LXwgQ3JlYXRlIGEgbmV3IGRpcmVjdG9yeSBhdCB0aGUgZ2l2ZW4gW1BhdGhdKEZpbGVTeXN0ZW0uUGF0aCNQYXRoKS5cblxuSWYgYHJlY3Vyc2l2ZWAgaXMgYFRydWVgLCB0aGVuIGEgZGlyZWN0b3J5IHdpbGwgYmUgY3JlYXRlZCBmb3IgZXZlcnkgc2VjdGlvbiBvZiB0aGVcbmdpdmVuIFtQYXRoXShGaWxlU3lzdGVtLlBhdGgjUGF0aCkuXG4tfVxubWFrZURpcmVjdG9yeSA6IFBlcm1pc3Npb24gLT4geyByZWN1cnNpdmUgOiBCb29sIH0gLT4gUGF0aCAtPiBUYXNrIEVycm9yIFBhdGhcbm1ha2VEaXJlY3RvcnkgXyBvcHRpb25zIHBhdGggPVxuICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0ubWFrZURpcmVjdG9yeSBvcHRpb25zIHBhdGhcblxuXG57LXwgQ3JlYXRlIGEgZGlyZWN0b3J5LCBwcmVmaXhlZCBieSBhIGdpdmVuIG5hbWUsIHRoYXQgZW5kcyB1cCBpbiBhIHNlY3Rpb24gb2YgdGhlXG5maWxlIHN5c3RlbSByZXNlcnZlZCBmb3IgdGVtcG9yYXJ5IGZpbGVzLiBZb3UncmUgZ2l2ZW4gdGhlIFtQYXRoXShGaWxlU3lzdGVtLlBhdGgjUGF0aClcbnRvIHRoaXMgbmV3IGRpcmVjdG9yeS5cbi19XG5tYWtlVGVtcERpcmVjdG9yeSA6IFBlcm1pc3Npb24gLT4gU3RyaW5nIC0+IFRhc2sgRXJyb3IgUGF0aFxubWFrZVRlbXBEaXJlY3RvcnkgXyBwcmVmaXggPVxuICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0ubWtkdGVtcCBwcmVmaXhcblxuXG4tLSBMSU5LU1xuXG5cbnstfCBDcmVhdGVzIGEgaGFyZCBsaW5rIGZyb20gdGhlIGxhc3QgW1BhdGhdKEZpbGVTeXN0ZW0uUGF0aCNQYXRoKSB0b1xudGhlIGZpcnN0LlxuXG5BIGhhcmQgbGluayBpcyBhbiBhbGlhcyBmb3IgYSBzcGVjaWZpYyBsb2NhdGlvbi4gVGhlIGxpbmsgaGFzIHRoZSBzYW1lXG5vd25lcnNoaXAgYW5kIGFjY2VzcyBwZXJtaXNzaW9ucywgYW5kIGl0J3MgaW1wb3NzaWJsZSB0byB0ZWxsIHdoaWNoIGlzXG50aGUgXCJyZWFsXCIgZW50aXR5IGFuZCB3aGljaCBpcyB0aGUgbGluay5cbi19XG5oYXJkTGluayA6IFBlcm1pc3Npb24gLT4gUGF0aCAtPiBQYXRoIC0+IFRhc2sgRXJyb3IgUGF0aFxuaGFyZExpbmsgXyBkZXN0IHNyYyA9XG4gICAgR3Jlbi5LZXJuZWwuRmlsZVN5c3RlbS5saW5rIHNyYyBkZXN0XG5cblxuey18IENyZWF0ZXMgYSBzb2Z0IGxpbmsgZnJvbSB0aGUgbGFzdCBbUGF0aF0oRmlsZVN5c3RlbS5QYXRoI1BhdGgpIHRvXG50aGUgZmlyc3QuXG5cbkEgc29mdCBsaW5rLCBhbHNvIGtub3duIGFzIGEgc3ltb2JsaWMgbGluayBvciBzeW1saW5rLCBpcyBhIHNwZWNpYWwgZmlsZVxudGhhdCBjb250YWlucyB0aGUgcGF0aCB0byBzb21lIG90aGVyIGxvY2F0aW9uLiBSZXNvbHZpbmcgYSBzb2Z0IGxpbmsgd2lsbFxucmVkaXJlY3QgdG8gdGhpcyBvdGhlciBsb2NhdGlvbi5cbi19XG5zb2Z0TGluayA6IFBlcm1pc3Npb24gLT4gUGF0aCAtPiBQYXRoIC0+IFRhc2sgRXJyb3IgUGF0aFxuc29mdExpbmsgXyBkZXN0IHNyYyA9XG4gICAgR3Jlbi5LZXJuZWwuRmlsZVN5c3RlbS5zeW1saW5rIHNyYyBkZXN0XG5cblxuey18IFJldHVybnMgdGhlIFtQYXRoXShGaWxlU3lzdGVtLlBhdGgjUGF0aCkgcG9pbnRlZCB0byBieSBhIHNvZnQgbGluay5cbi19XG5yZWFkTGluayA6IFBlcm1pc3Npb24gLT4gUGF0aCAtPiBUYXNrIEVycm9yIFBhdGhcbnJlYWRMaW5rIF8gcGF0aCA9XG4gICAgR3Jlbi5LZXJuZWwuRmlsZVN5c3RlbS5yZWFkTGluayBwYXRoXG5cblxuey18IFJlbW92ZXMgYSBsaW5rLCBoYXJkIG9yIHNvZnQsIGZyb20gdGhlIGZpbGUgc3lzdGVtLiBJZiB0aGVcbltQYXRoXShGaWxlU3lzdGVtLlBhdGgjUGF0aCkgcmVmZXJzIHRvIGEgZmlsZSwgdGhlIGZpbGUgaXMgcmVtb3ZlZC5cbi19XG51bmxpbmsgOiBQZXJtaXNzaW9uIC0+IFBhdGggLT4gVGFzayBFcnJvciBQYXRoXG51bmxpbmsgXyBwYXRoID1cbiAgICBHcmVuLktlcm5lbC5GaWxlU3lzdGVtLnVubGluayBwYXRoXG5cblxuLS0gU1VCU0NSSVBUSU9OXG5cblxudHlwZSBGaWxlU3lzdGVtU3ViIG1zZ1xuICAgID0gV2F0Y2ggUGF0aCBCb29sIChXYXRjaEV2ZW50IC0+IG1zZylcblxuXG57LXwgUmVwcmVzZW50cyBhIGNoYW5nZSB3aXRoaW4gYSB3YXRjaGVkIGRpcmVjdG9yeS5cblxuKiBgQ2hhbmdlZGAgbWVhbnMgdGhhdCB0aGUgY29udGVudHMgb2YgYSBmaWxlIGhhcyBjaGFuZ2VkIGluIHNvbWUgd2F5LlxuKiBgTW92ZWRgIG1lYW5zIHRoYXQgYW4gZW50aXR5IGhhcyBiZWVuIGFkZGVkIG9yIHJlbW92ZWQuIEEgcmVuYW1lIGlzIHVzdWFsbHkgdHdvIGBNb3ZlZGAgZXZlbnRzLlxuXG5PbiBtb3N0IG9wZXJhdGluZyBzeXN0ZW1zLCBlYWNoIGV2ZW50IHdpbGwgYmUgYXNzb2NpYXRlZCB3aXRoIGEgW1BhdGhdKEZpbGVTeXN0ZW0uUGF0aCNQYXRoKVxucmVsYXRpdmUgdG8gdGhlIHdhdGNoZWQgZGlyZWN0b3J5LCBidXQgc29tZSBvcGVyYXRpbmcgc3lzdGVtcyB3aWxsIG5vdCBwcm92aWRlIHRoYXQgaW5mb3JtYXRpb24uXG4tfVxudHlwZSBXYXRjaEV2ZW50XG4gICAgPSBDaGFuZ2VkIChNYXliZSBQYXRoKVxuICAgIHwgTW92ZWQgKE1heWJlIFBhdGgpXG5cblxuc3ViTWFwIDogKGEgLT4gYikgLT4gRmlsZVN5c3RlbVN1YiBhIC0+IEZpbGVTeXN0ZW1TdWIgYlxuc3ViTWFwIG1hcEZuIHN1YiA9XG4gICAgY2FzZSBzdWIgb2ZcbiAgICAgICAgV2F0Y2ggcGF0aCByZWN1cnNpdmUgbXNnTWFwIC0+XG4gICAgICAgICAgICBXYXRjaCBwYXRoIHJlY3Vyc2l2ZSAobWFwRm4gPDwgbXNnTWFwKVxuXG5cbnstfCBUaGlzIG5vdGlmaWVzIHlvdXIgYXBwbGljYXRpb24gZXZlcnkgdGltZSB0aGVyZSBpcyBhIGNoYW5nZSB3aXRoaW4gdGhlIGRpcmVjdG9yeVxucmVwcmVzZW50ZWQgYnkgdGhlIGdpdmVuIFtQYXRoXShGaWxlU3lzdGVtLlBhdGgjUGF0aCkuXG4tfVxud2F0Y2ggOiBQZXJtaXNzaW9uIC0+IChXYXRjaEV2ZW50IC0+IG1zZykgLT4gUGF0aCAtPiBTdWIgbXNnXG53YXRjaCBfIG1zZ01hcCBwYXRoID1cbiAgICBzdWJzY3JpcHRpb24gKFdhdGNoIHBhdGggRmFsc2UgbXNnTWFwKVxuXG5cbnstfCBTYW1lIGFzIFt3YXRjaF0oI3dhdGNoKSwgYnV0IHRoaXMgd2lsbCBhbHNvIHdhdGNoIGZvciBjaGFuZ2VzIGluIHN1Yi1kaXJlY3Rvcmllcy5cbi19XG53YXRjaFJlY3Vyc2l2ZSA6IFBlcm1pc3Npb24gLT4gKFdhdGNoRXZlbnQgLT4gbXNnKSAtPiBQYXRoIC0+IFN1YiBtc2dcbndhdGNoUmVjdXJzaXZlIF8gbXNnTWFwIHBhdGggPVxuICAgIHN1YnNjcmlwdGlvbiAoV2F0Y2ggcGF0aCBUcnVlIG1zZ01hcClcblxuXG4tLSBMT09QXG5cblxudHlwZSBhbGlhcyBTdGF0ZSBtc2cgPVxuICAgIHsgd2F0Y2hlcnMgOiBEaWN0IFN0cmluZyAoVGFnZ2VycyBtc2cpXG4gICAgLCByZWN1cnNpdmVXYXRjaGVycyA6IERpY3QgU3RyaW5nIChUYWdnZXJzIG1zZylcbiAgICAsIHByb2Nlc3NlcyA6IERpY3QgU3RyaW5nIFByb2Nlc3MuSWRcbiAgICB9XG5cblxudHlwZSBhbGlhcyBUYWdnZXJzIG1zZyA9XG4gICAgQXJyYXkgKFdhdGNoRXZlbnQgLT4gbXNnKVxuXG5cbmluaXQgOiBUYXNrIE5ldmVyIChTdGF0ZSBtc2cpXG5pbml0ID1cbiAgICBUYXNrLnN1Y2NlZWRcbiAgICAgICAgeyB3YXRjaGVycyA9IERpY3QuZW1wdHlcbiAgICAgICAgLCByZWN1cnNpdmVXYXRjaGVycyA9IERpY3QuZW1wdHlcbiAgICAgICAgLCBwcm9jZXNzZXMgPSBEaWN0LmVtcHR5XG4gICAgICAgIH1cblxuXG5vbkVmZmVjdHNcbiAgICA6IFBsYXRmb3JtLlJvdXRlciBtc2cgU2VsZk1zZ1xuICAgIC0+IEFycmF5IChGaWxlU3lzdGVtU3ViIG1zZylcbiAgICAtPiBTdGF0ZSBtc2dcbiAgICAtPiBUYXNrLlRhc2sgTmV2ZXIgKFN0YXRlIG1zZylcbm9uRWZmZWN0cyByb3V0ZXIgc3VicyBzdGF0ZSA9XG4gICAgbGV0XG4gICAgICAgIG5ld1dhdGNoZXJzID1cbiAgICAgICAgICAgIHN1YnNcbiAgICAgICAgICAgICAgICB8PiBBcnJheS5maWx0ZXIgKFxcKFdhdGNoIF8gYm9vbCBfKSAtPiBub3QgYm9vbClcbiAgICAgICAgICAgICAgICB8PiBBcnJheS5mb2xkbCBzdWJUb1dhdGNoZXIgRGljdC5lbXB0eVxuXG4gICAgICAgIG5ld1JlY3Vyc2l2ZVdhdGNoZXJzID1cbiAgICAgICAgICAgIHN1YnNcbiAgICAgICAgICAgICAgICB8PiBBcnJheS5maWx0ZXIgKFxcKFdhdGNoIF8gYm9vbCBfKSAtPiBib29sKVxuICAgICAgICAgICAgICAgIHw+IEFycmF5LmZvbGRsIHN1YlRvV2F0Y2hlciBEaWN0LmVtcHR5XG5cbiAgICAgICAgdG9TcGF3biA9XG4gICAgICAgICAgICBEaWN0LmRpZmYgbmV3V2F0Y2hlcnMgc3RhdGUud2F0Y2hlcnNcbiAgICAgICAgICAgICAgICB8PiBEaWN0LmtleXNcbiAgICAgICAgXG4gICAgICAgIHJlY3Vyc2l2ZVRvU3Bhd24gPVxuICAgICAgICAgICAgRGljdC5kaWZmIG5ld1JlY3Vyc2l2ZVdhdGNoZXJzIHN0YXRlLnJlY3Vyc2l2ZVdhdGNoZXJzXG4gICAgICAgICAgICAgICAgfD4gRGljdC5rZXlzXG4gICAgICAgIFxuICAgICAgICB0b1N0b3AgPVxuICAgICAgICAgICAgRGljdC5kaWZmIHN0YXRlLndhdGNoZXJzIG5ld1dhdGNoZXJzXG4gICAgICAgICAgICAgICAgfD4gRGljdC5rZXlzXG4gICAgICAgIFxuICAgICAgICByZWN1cnNpdmVUb1N0b3AgPVxuICAgICAgICAgICAgRGljdC5kaWZmIHN0YXRlLnJlY3Vyc2l2ZVdhdGNoZXJzIG5ld1JlY3Vyc2l2ZVdhdGNoZXJzXG4gICAgICAgICAgICAgICAgfD4gRGljdC5rZXlzXG5cbiAgICAgICAgYXNSZWN1cnNpdmVLZXkga2V5ID1cbiAgICAgICAgICAgIGtleSArKyBcIiRyZWN1cnNpdmVcIlxuXG4gICAgICAgIGZsaXBGb2xkIGZuIGFycmF5IGluaXRpYWwgPVxuICAgICAgICAgICAgQXJyYXkuZm9sZGwgZm4gaW5pdGlhbCBhcnJheVxuXG4gICAgICAgIHRhc2tTdG9wcGVyIGtleU1hcHBlciA9XG4gICAgICAgICAgICAoXFxrZXkgYWNjVGFzayAtPlxuICAgICAgICAgICAgICAgIFRhc2suYW5kVGhlbiBcbiAgICAgICAgICAgICAgICAgICAgKFxccHJvY2Vzc2VzIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICBsZXRcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICByZWFsS2V5ID0gXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGtleU1hcHBlciBrZXlcbiAgICAgICAgICAgICAgICAgICAgICAgIGluXG4gICAgICAgICAgICAgICAgICAgICAgICBjYXNlIERpY3QuZ2V0IHJlYWxLZXkgcHJvY2Vzc2VzIG9mXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgSnVzdCBwcm9jZXNzSWQgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgUHJvY2Vzcy5raWxsIHByb2Nlc3NJZFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5tYXAgKFxcXyAtPiBEaWN0LnJlbW92ZSByZWFsS2V5IHByb2Nlc3NlcylcblxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgYWNjVGFza1xuICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICAgICAgICAgIGFjY1Rhc2tcbiAgICAgICAgICAgIClcblxuICAgICAgICB0YXNrU3Bhd25lciBrZXlNYXBwZXIgaXNSZWN1cnNpdmUgPVxuICAgICAgICAgICAgKFxca2V5IGFjY1Rhc2sgLT5cbiAgICAgICAgICAgICAgICBhY2NUYXNrXG4gICAgICAgICAgICAgICAgICAgIHw+IFRhc2suYW5kVGhlbiBcbiAgICAgICAgICAgICAgICAgICAgICAgIChcXHByb2Nlc3NlcyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIFByb2Nlc3Muc3Bhd24gKGF0dGFjaFdhdGNoZXIga2V5IGlzUmVjdXJzaXZlIChQbGF0Zm9ybS5zZW5kVG9TZWxmIHJvdXRlciA8PCBXYXRjaFBhdGhDaGFuZ2UgaXNSZWN1cnNpdmUga2V5KSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5tYXAgKFxcaWQgLT4gRGljdC5zZXQgKGtleU1hcHBlciBrZXkpIGlkIHByb2Nlc3NlcylcbiAgICAgICAgICAgICAgICAgICAgICAgIClcbiAgICAgICAgICAgIClcbiAgICBpblxuICAgIEFycmF5LmZvbGRsIFxuICAgICAgICAodGFza1N0b3BwZXIgaWRlbnRpdHkpXG4gICAgICAgIChUYXNrLnN1Y2NlZWQgc3RhdGUucHJvY2Vzc2VzKVxuICAgICAgICB0b1N0b3BcbiAgICB8PiBmbGlwRm9sZCAodGFza1N0b3BwZXIgYXNSZWN1cnNpdmVLZXkpIHJlY3Vyc2l2ZVRvU3RvcFxuICAgIHw+IGZsaXBGb2xkICh0YXNrU3Bhd25lciBpZGVudGl0eSBGYWxzZSkgdG9TcGF3blxuICAgIHw+IGZsaXBGb2xkICh0YXNrU3Bhd25lciBhc1JlY3Vyc2l2ZUtleSBUcnVlKSByZWN1cnNpdmVUb1NwYXduXG4gICAgfD4gVGFzay5tYXBcbiAgICAgICAgKFxcbmV3UHJvY2Vzc2VzIC0+XG4gICAgICAgICAgICB7IHdhdGNoZXJzID0gbmV3V2F0Y2hlcnNcbiAgICAgICAgICAgICwgcmVjdXJzaXZlV2F0Y2hlcnMgPSBuZXdSZWN1cnNpdmVXYXRjaGVyc1xuICAgICAgICAgICAgLCBwcm9jZXNzZXMgPSBuZXdQcm9jZXNzZXNcbiAgICAgICAgICAgIH1cbiAgICAgICAgKVxuXG5cbnN1YlRvV2F0Y2hlciA6IEZpbGVTeXN0ZW1TdWIgbXNnIC0+IERpY3QgU3RyaW5nIChUYWdnZXJzIG1zZykgLT4gRGljdCBTdHJpbmcgKFRhZ2dlcnMgbXNnKVxuc3ViVG9XYXRjaGVyIHN1YiB0YWdnZXJzID1cbiAgICBjYXNlIHN1YiBvZlxuICAgICAgICBXYXRjaCBwYXRoIF8gdGFnZ2VyIC0+XG4gICAgICAgICAgICBsZXRcbiAgICAgICAgICAgICAgICBrZXkgPVxuICAgICAgICAgICAgICAgICAgICBHcmVuLktlcm5lbC5GaWxlUGF0aC50b1N0cmluZyBwYXRoXG4gICAgICAgICAgICBpblxuICAgICAgICAgICAgY2FzZSBEaWN0LmdldCBrZXkgdGFnZ2VycyBvZlxuICAgICAgICAgICAgICAgIEp1c3QgZGF0YSAtPlxuICAgICAgICAgICAgICAgICAgICBEaWN0LnNldCBcbiAgICAgICAgICAgICAgICAgICAgICAgIGtleVxuICAgICAgICAgICAgICAgICAgICAgICAgKEFycmF5LnB1c2hMYXN0IHRhZ2dlciBkYXRhKVxuICAgICAgICAgICAgICAgICAgICAgICAgdGFnZ2Vyc1xuXG4gICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICBEaWN0LnNldCBrZXkgWyB0YWdnZXIgXSB0YWdnZXJzXG5cblxuYXR0YWNoV2F0Y2hlciA6IFN0cmluZyAtPiBCb29sIC0+IChXYXRjaEV2ZW50IC0+IFRhc2suVGFzayBOZXZlciB7fSkgLT4gVGFzay5UYXNrIHgge31cbmF0dGFjaFdhdGNoZXIgPVxuICBHcmVuLktlcm5lbC5GaWxlU3lzdGVtLndhdGNoXG5cblxudHlwZSBTZWxmTXNnXG4gICAgPSBXYXRjaFBhdGhDaGFuZ2UgQm9vbCBTdHJpbmcgV2F0Y2hFdmVudFxuXG5cbm9uU2VsZk1zZyA6IFBsYXRmb3JtLlJvdXRlciBtc2cgU2VsZk1zZyAtPiBTZWxmTXNnIC0+IFN0YXRlIG1zZyAtPiBUYXNrLlRhc2sgTmV2ZXIgKFN0YXRlIG1zZylcbm9uU2VsZk1zZyByb3V0ZXIgKFdhdGNoUGF0aENoYW5nZSByZWN1cnNpdmUga2V5IGV2ZW50KSBzdGF0ZSA9XG4gICAgbGV0XG4gICAgICAgIG5vdGlmeUFwcGxpY2F0aW9uIHRhZ2dlcnMgPVxuICAgICAgICAgICAgQXJyYXkuZm9sZGwgXG4gICAgICAgICAgICAgICAgKFxcdGFnZ2VyIHRhc2tzIC0+IFxuICAgICAgICAgICAgICAgICAgICB0YXNrc1xuICAgICAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5hbmRUaGVuIChcXHt9IC0+IFBsYXRmb3JtLnNlbmRUb0FwcCByb3V0ZXIgKHRhZ2dlciBldmVudCkpXG4gICAgICAgICAgICAgICAgKVxuICAgICAgICAgICAgICAgIChUYXNrLnN1Y2NlZWQge30pXG4gICAgICAgICAgICAgICAgdGFnZ2Vyc1xuICAgICAgICAgICAgICAgIHw+IFRhc2subWFwIChcXHt9IC0+IHN0YXRlKVxuICAgIGluXG4gICAgaWYgcmVjdXJzaXZlIHRoZW5cbiAgICAgICAgY2FzZSBEaWN0LmdldCBrZXkgc3RhdGUucmVjdXJzaXZlV2F0Y2hlcnMgb2ZcbiAgICAgICAgICAgIEp1c3QgdGFnZ2VycyAtPlxuICAgICAgICAgICAgICAgIG5vdGlmeUFwcGxpY2F0aW9uIHRhZ2dlcnNcblxuICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgIFRhc2suc3VjY2VlZCBzdGF0ZVxuXG4gICAgZWxzZVxuICAgICAgICBjYXNlIERpY3QuZ2V0IGtleSBzdGF0ZS53YXRjaGVycyBvZlxuICAgICAgICAgICAgSnVzdCB0YWdnZXJzIC0+XG4gICAgICAgICAgICAgICAgbm90aWZ5QXBwbGljYXRpb24gdGFnZ2Vyc1xuXG4gICAgICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICAgICAgVGFzay5zdWNjZWVkIHN0YXRlXG5cblxuLS0gU1BFQ0lBTCBQQVRIU1xuXG5cbnstfCBGaW5kIHRoZSBbUGF0aF0oRmlsZVN5c3RlbS5QYXRoI1BhdGgpIHRoYXQgcmVwcmVzZW50cyB0aGUgaG9tZSBkaXJlY3Rvcnkgb2YgdGhlIGN1cnJlbnQgdXNlci5cbi19XG5ob21lRGlyZWN0b3J5IDogUGVybWlzc2lvbiAtPiBUYXNrIHggUGF0aFxuaG9tZURpcmVjdG9yeSBfID1cbiAgICBHcmVuLktlcm5lbC5GaWxlU3lzdGVtLmhvbWVEaXJcblxuXG57LXwgUmV0dXJucyB0aGUgY3VycmVudCB3b3JraW5nIGRpcmVjdG9yeSBvZiB0aGUgcHJvZ3JhbS5cblxuVGhpcyBpcyB0aGUgZGlyZWN0b3J5IHRoYXQgYWxsIHJlbGF0aXZlIHBhdGhzIGFyZSByZWxhdGl2ZSB0bywgYW5kIGlzIHVzdWFsbHkgdGhlXG5kaXJlY3RvcnkgdGhhdCB0aGUgcHJvZ3JhbSB3YXMgZXhlY3V0ZWQgZnJvbS5cbi19XG5jdXJyZW50V29ya2luZ0RpcmVjdG9yeSA6IFBlcm1pc3Npb24gLT4gVGFzayB4IFBhdGhcbmN1cnJlbnRXb3JraW5nRGlyZWN0b3J5IF8gPVxuICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0uY3VycmVudFdvcmtpbmdEaXJlY3RvcnlcblxuXG57LXwgRmluZCBhIFtQYXRoXShGaWxlU3lzdGVtLlBhdGgjUGF0aCkgdGhhdCByZXByZXNlbnRzIGEgZGlyZWN0b3J5IG1lYW50IHRvIGhvbGQgdGVtcG9yYXJ5IGZpbGVzLlxuLX1cbnRtcERpcmVjdG9yeSA6IFBlcm1pc3Npb24gLT4gVGFzayB4IFBhdGhcbnRtcERpcmVjdG9yeSBfID1cbiAgICBHcmVuLktlcm5lbC5GaWxlU3lzdGVtLnRtcERpclxuXG5cbnstfCBbUGF0aF0oRmlsZVN5c3RlbS5QYXRoI1BhdGgpIHRvIGEgZmlsZSB3aGljaCBpcyBhbHdheXMgZW1wdHkuIEFueXRoaW5nIHdyaXR0ZW4gdG8gdGhpcyBmaWxlIHdpbGwgYmUgZGlzY2FyZGVkLlxuLX1cbmRldk51bGwgOiBQZXJtaXNzaW9uIC0+IFRhc2sgeCBQYXRoXG5kZXZOdWxsIF8gPVxuICAgIEdyZW4uS2VybmVsLkZpbGVTeXN0ZW0uZGV2TnVsbFxuIiwKICAgICAgICAibW9kdWxlIE1haW4gZXhwb3NpbmcgKG1haW4pXG5cbmltcG9ydCBOb2RlXG5pbXBvcnQgQ2hpbGRQcm9jZXNzXG5pbXBvcnQgSW5pdFxuaW1wb3J0IFN0cmVhbSBleHBvc2luZyAoU3RyZWFtKVxuaW1wb3J0IFRhc2sgZXhwb3NpbmcgKFRhc2spXG5pbXBvcnQgRGljdCBleHBvc2luZyAoRGljdClcbmltcG9ydCBGaWxlU3lzdGVtXG5pbXBvcnQgRmlsZVN5c3RlbS5QYXRoIGFzIFBhdGggZXhwb3NpbmcgKFBhdGgpXG5pbXBvcnQgSHR0cENsaWVudFxuaW1wb3J0IEJ5dGVzIGV4cG9zaW5nIChCeXRlcylcbmltcG9ydCBUZXJtaW5hbFxuaW1wb3J0IFByb2Nlc3NcblxuXG5tYWluIDogTm9kZS5Qcm9ncmFtIE1vZGVsIE1zZ1xubWFpbiA9XG4gICAgTm9kZS5kZWZpbmVQcm9ncmFtXG4gICAgICAgIHsgaW5pdCA9IGluaXRcbiAgICAgICAgLCB1cGRhdGUgPSB1cGRhdGVcbiAgICAgICAgLCBzdWJzY3JpcHRpb25zID0gXFxfbW9kZWwgLT4gU3ViLm5vbmVcbiAgICAgICAgfVxuXG5cbnR5cGUgYWxpYXMgTW9kZWwgPSBcbiAgICB7IGFyZ3MgOiBBcnJheSBTdHJpbmdcbiAgICAsIHN0ZG91dCA6IFN0cmVhbVxuICAgICwgc3RkZXJyIDogU3RyZWFtXG4gICAgLCB1c2VDb2xvciA6IEJvb2xcbiAgICAsIGZzUGVybWlzc2lvbiA6IEZpbGVTeXN0ZW0uUGVybWlzc2lvblxuICAgICwgY3BQZXJtaXNzaW9uIDogQ2hpbGRQcm9jZXNzLlBlcm1pc3Npb25cbiAgICAsIGh0dHBQZXJtaXNzaW9uIDogSHR0cENsaWVudC5QZXJtaXNzaW9uXG4gICAgLCByZW1vdGVQYXRoIDogTWF5YmUgU3RyaW5nXG4gICAgLCBsb2NhbFBhdGggOiBQYXRoXG4gICAgLCBwYXRoVG9TdHJpbmcgOiBQYXRoIC0+IFN0cmluZ1xuICAgIH1cblxuXG5jb21waWxlclZlcnNpb24gOiBTdHJpbmdcbmNvbXBpbGVyVmVyc2lvbiA9XG4gICAgXCIwLjQuNFwiXG5cbmNvdW50RG93biA6IEludCAtPiB7fSAtPiBJbnRcbmNvdW50RG93biBuIHJlYyA9XG4gICAgaWYgbiA8PSAwIHRoZW5cbiAgICAgICAgMFxuXG4gICAgZWxzZVxuICAgICAgICBjb3VudERvd24gKG4gLSAxKSByZWNcblxuaW5pdCA6IE5vZGUuRW52aXJvbm1lbnQgLT4gSW5pdC5UYXNrIHsgbW9kZWwgOiBNb2RlbCwgY29tbWFuZCA6IENtZCBNc2cgfVxuaW5pdCBlbnYgPVxuICAgIEluaXQuYXdhaXQgRmlsZVN5c3RlbS5pbml0aWFsaXplIDx8IFxcZnNQZXJtaXNzaW9uIC0+XG4gICAgSW5pdC5hd2FpdCBDaGlsZFByb2Nlc3MuaW5pdGlhbGl6ZSA8fCBcXGNwUGVybWlzc2lvbiAtPlxuICAgIEluaXQuYXdhaXQgSHR0cENsaWVudC5pbml0aWFsaXplIDx8IFxcaHR0cFBlcm1pc3Npb24gLT5cbiAgICBJbml0LmF3YWl0IFRlcm1pbmFsLmluaXRpYWxpemUgPHwgXFx0ZXJtaW5hbENvbmZpZyAtPlxuICAgIEluaXQuYXdhaXRUYXNrIE5vZGUuZ2V0RW52aXJvbm1lbnRWYXJpYWJsZXMgPHwgXFxlbnZWYXJzIC0+XG4gICAgSW5pdC5hd2FpdFRhc2sgKEZpbGVTeXN0ZW0uaG9tZURpcmVjdG9yeSBmc1Blcm1pc3Npb24pIDx8IFxcaG9tZURpciAtPlxuICAgICAgICAgICAgbGV0XG4gICAgICAgICAgICAgICAgdXNlckFyZ3MgPVxuICAgICAgICAgICAgICAgICAgICBBcnJheS5kcm9wRmlyc3QgMiBlbnYuYXJnc1xuXG4gICAgICAgICAgICAgICAgdXNlbGVzcyA9IGNvdW50RG93biAxMCB7fVxuXG4gICAgICAgICAgICAgICAgdXNlQ29sb3IgPVxuICAgICAgICAgICAgICAgICAgICBjYXNlIHRlcm1pbmFsQ29uZmlnIG9mXG4gICAgICAgICAgICAgICAgICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgRmFsc2VcblxuICAgICAgICAgICAgICAgICAgICAgICAgSnVzdCBfIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgY2FzZSBEaWN0LmdldCBcIk5PX0NPTE9SXCIgZW52VmFycyBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBKdXN0IF8gLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEZhbHNlXG5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgVHJ1ZVxuXG4gICAgICAgICAgICAgICAgbWF5YmVQYXRocyA9XG4gICAgICAgICAgICAgICAgICAgIGNhc2UgeyBwbGF0Zm9ybSA9IGVudi5wbGF0Zm9ybSwgYXJjaCA9IGVudi5jcHVBcmNoaXRlY3R1cmUsIG92ZXJyaWRlID0gRGljdC5nZXQgXCJHUkVOX0JJTlwiIGVudlZhcnMgfSBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgeyBvdmVycmlkZSA9IEp1c3Qgb3ZlcnJpZGVQYXRoLCBwbGF0Zm9ybSA9IE5vZGUuV2luMzIgfSAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEp1c3QgPHxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgeyBhcmdzID0gdXNlckFyZ3NcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBzdGRvdXQgPSBlbnYuc3Rkb3V0XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgcmVtb3RlUGF0aCA9IE5vdGhpbmdcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBsb2NhbFBhdGggPSBQYXRoLmZyb21XaW4zMlN0cmluZyBvdmVycmlkZVBhdGhcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgXG4gICAgICAgICAgICAgICAgICAgICAgICB7IG92ZXJyaWRlID0gSnVzdCBvdmVycmlkZVBhdGggfSAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEp1c3QgPHxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgeyBhcmdzID0gdXNlckFyZ3NcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBzdGRvdXQgPSBlbnYuc3Rkb3V0XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgcmVtb3RlUGF0aCA9IE5vdGhpbmdcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBsb2NhbFBhdGggPSBQYXRoLmZyb21Qb3NpeFN0cmluZyBvdmVycmlkZVBhdGhcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcbiAgICAgICAgICAgICAgICAgICAgICAgIHsgcGxhdGZvcm0gPSBOb2RlLldpbjMyLCBhcmNoID0gTm9kZS5YNjQgfSAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEp1c3QgPHxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgeyBhcmdzID0gdXNlckFyZ3NcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBzdGRvdXQgPSBlbnYuc3Rkb3V0XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgcmVtb3RlUGF0aCA9IEp1c3QgPHwgbWFrZVJlbW90ZVBhdGggXCJncmVuLmV4ZVwiXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgbG9jYWxQYXRoID0gbWFrZUxvY2FsUGF0aCBlbnYucGxhdGZvcm0gaG9tZURpciBlbnZWYXJzXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgICAgICAgICAgICAgeyBwbGF0Zm9ybSA9IE5vZGUuRGFyd2luIH0gLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBKdXN0IDx8XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHsgYXJncyA9IHVzZXJBcmdzXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgc3Rkb3V0ID0gZW52LnN0ZG91dFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAsIHJlbW90ZVBhdGggPSBKdXN0IDx8IG1ha2VSZW1vdGVQYXRoIFwiZ3Jlbl9tYWNcIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAsIGxvY2FsUGF0aCA9IG1ha2VMb2NhbFBhdGggZW52LnBsYXRmb3JtIGhvbWVEaXIgZW52VmFyc1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG5cbiAgICAgICAgICAgICAgICAgICAgICAgIHsgcGxhdGZvcm0gPSBOb2RlLkxpbnV4LCBhcmNoID0gTm9kZS5YNjQgfSAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIEp1c3QgPHxcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgeyBhcmdzID0gdXNlckFyZ3NcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBzdGRvdXQgPSBlbnYuc3Rkb3V0XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgcmVtb3RlUGF0aCA9IEp1c3QgPHwgbWFrZVJlbW90ZVBhdGggXCJncmVuX2xpbnV4XCJcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBsb2NhbFBhdGggPSBtYWtlTG9jYWxQYXRoIGVudi5wbGF0Zm9ybSBob21lRGlyIGVudlZhcnNcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuXG4gICAgICAgICAgICAgICAgICAgICAgICBfIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgTm90aGluZ1xuXG4gICAgICAgICAgICAgICAgbW9kZWwgPVxuICAgICAgICAgICAgICAgICAgICBjYXNlIG1heWJlUGF0aHMgb2ZcbiAgICAgICAgICAgICAgICAgICAgICAgIEp1c3QgcGF0aHMgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB7IGFyZ3MgPSB1c2VyQXJnc1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgc3Rkb3V0ID0gZW52LnN0ZG91dFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgc3RkZXJyID0gZW52LnN0ZGVyclxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgdXNlQ29sb3IgPSB1c2VDb2xvclxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgZnNQZXJtaXNzaW9uID0gZnNQZXJtaXNzaW9uXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBjcFBlcm1pc3Npb24gPSBjcFBlcm1pc3Npb25cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAsIGh0dHBQZXJtaXNzaW9uID0gaHR0cFBlcm1pc3Npb25cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAsIHJlbW90ZVBhdGggPSBwYXRocy5yZW1vdGVQYXRoXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBsb2NhbFBhdGggPSBwYXRocy5sb2NhbFBhdGhcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAsIHBhdGhUb1N0cmluZyA9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIGlmIGVudi5wbGF0Zm9ybSA9PSBOb2RlLldpbjMyIHRoZW5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFBhdGgudG9XaW4zMlN0cmluZ1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBlbHNlXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBQYXRoLnRvUG9zaXhTdHJpbmdcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgICAgICBcbiAgICAgICAgICAgICAgICAgICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAtLSBkdW1teSBtb2RlbFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHsgYXJncyA9IFtdXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBzdGRvdXQgPSBlbnYuc3Rkb3V0XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBzdGRlcnIgPSBlbnYuc3RkZXJyXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLCB1c2VDb2xvciA9IHVzZUNvbG9yXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBmc1Blcm1pc3Npb24gPSBmc1Blcm1pc3Npb25cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAsIGNwUGVybWlzc2lvbiA9IGNwUGVybWlzc2lvblxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgaHR0cFBlcm1pc3Npb24gPSBodHRwUGVybWlzc2lvblxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgcmVtb3RlUGF0aCA9IE5vdGhpbmdcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAsIGxvY2FsUGF0aCA9IFBhdGguZW1wdHlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAsIHBhdGhUb1N0cmluZyA9IFBhdGgudG9Qb3NpeFN0cmluZ1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgIGluXG4gICAgICAgICAgICBOb2RlLnN0YXJ0UHJvZ3JhbVxuICAgICAgICAgICAgICAgIHsgbW9kZWwgPSBtb2RlbFxuICAgICAgICAgICAgICAgICwgY29tbWFuZCA9XG4gICAgICAgICAgICAgICAgICAgIGNhc2UgbWF5YmVQYXRocyBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgSnVzdCBfIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgRmlsZVN5c3RlbS5jaGVja0FjY2VzcyBmc1Blcm1pc3Npb24gW10gbW9kZWwubG9jYWxQYXRoXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHw+IFRhc2suYXR0ZW1wdCBFeGlzdGFuY2VDaGVja2VkXG5cbiAgICAgICAgICAgICAgICAgICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBTdHJlYW0uc2VuZExpbmUgZW52LnN0ZGVyciBcIldlIGN1cnJlbnRseSBkb24ndCBzdXBwb3J0IHRoaXMgcGxhdGZvcm0vYXJjaC5cIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB8PiBUYXNrLmV4ZWN1dGVcbiAgICAgICAgICAgICAgICB9XG5cblxubWFrZVJlbW90ZVBhdGggOiBTdHJpbmcgLT4gU3RyaW5nXG5tYWtlUmVtb3RlUGF0aCBmaWxlbmFtZSA9XG4gICAgU3RyaW5nLmpvaW4gXCIvXCJcbiAgICAgICAgWyBcImh0dHBzOi8vZ2l0aHViLmNvbS9ncmVuLWxhbmcvY29tcGlsZXIvcmVsZWFzZXMvZG93bmxvYWRcIlxuICAgICAgICAsIGNvbXBpbGVyVmVyc2lvblxuICAgICAgICAsIGZpbGVuYW1lXG4gICAgICAgIF1cblxuXG5tYWtlTG9jYWxQYXRoIDogTm9kZS5QbGF0Zm9ybSAtPiBQYXRoIC0+IERpY3QgU3RyaW5nIFN0cmluZyAtPiBQYXRoXG5tYWtlTG9jYWxQYXRoIHBsYXRmb3JtIGhvbWVEaXIgZW52VmFycyA9XG4gICAgbGV0XG4gICAgICAgIHN0YXJ0UGF0aCA9XG4gICAgICAgICAgICBjYXNlIHBsYXRmb3JtIG9mXG4gICAgICAgICAgICAgICAgTm9kZS5XaW4zMiAtPlxuICAgICAgICAgICAgICAgICAgICBlbnZWYXJzXG4gICAgICAgICAgICAgICAgICAgICAgICB8PiBEaWN0LmdldCBcIkxPQ0FMQVBQREFUQVwiXG4gICAgICAgICAgICAgICAgICAgICAgICB8PiBNYXliZS5tYXAgUGF0aC5mcm9tV2luMzJTdHJpbmdcbiAgICAgICAgICAgICAgICAgICAgICAgIHw+IE1heWJlLndpdGhEZWZhdWx0IChcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIkFwcERhdGEvTG9jYWxcIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB8PiBQYXRoLmZyb21Qb3NpeFN0cmluZ1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB8PiBQYXRoLnByZXBlbmQgaG9tZURpclxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIClcblxuICAgICAgICAgICAgICAgIE5vZGUuRGFyd2luIC0+XG4gICAgICAgICAgICAgICAgICAgIFwiTGlicmFyeS9DYWNoZXNcIlxuICAgICAgICAgICAgICAgICAgICAgICAgfD4gUGF0aC5mcm9tUG9zaXhTdHJpbmdcbiAgICAgICAgICAgICAgICAgICAgICAgIHw+IFBhdGgucHJlcGVuZCBob21lRGlyXG5cbiAgICAgICAgICAgICAgICBfIC0+XG4gICAgICAgICAgICAgICAgICAgIGVudlZhcnNcbiAgICAgICAgICAgICAgICAgICAgICAgIHw+IERpY3QuZ2V0IFwiWERHX0NBQ0hFX0hPTUVcIlxuICAgICAgICAgICAgICAgICAgICAgICAgfD4gTWF5YmUubWFwIFBhdGguZnJvbVBvc2l4U3RyaW5nXG4gICAgICAgICAgICAgICAgICAgICAgICB8PiBNYXliZS53aXRoRGVmYXVsdCAoUGF0aC5hcHBlbmQgKFBhdGguZnJvbVBvc2l4U3RyaW5nIFwiLmNhY2hlXCIpIGhvbWVEaXIpXG5cbiAgICAgICAgZmlsZW5hbWUgPVxuICAgICAgICAgICAgY2FzZSBwbGF0Zm9ybSBvZlxuICAgICAgICAgICAgICAgIE5vZGUuV2luMzIgLT5cbiAgICAgICAgICAgICAgICAgICAgXCJncmVuLmV4ZVwiXG5cbiAgICAgICAgICAgICAgICBfIC0+XG4gICAgICAgICAgICAgICAgICAgIFwiZ3JlblwiXG4gICAgICAgIFxuICAgICAgICBlbmRQYXRoID1cbiAgICAgICAgICAgIFsgXCJncmVuXCJcbiAgICAgICAgICAgICwgY29tcGlsZXJWZXJzaW9uXG4gICAgICAgICAgICAsIFwiYmluXCJcbiAgICAgICAgICAgICwgZmlsZW5hbWVcbiAgICAgICAgICAgIF1cbiAgICAgICAgICAgICAgICB8PiBTdHJpbmcuam9pbiBcIi9cIlxuICAgICAgICAgICAgICAgIHw+IFBhdGguZnJvbVBvc2l4U3RyaW5nXG4gICAgaW5cbiAgICBQYXRoLnByZXBlbmQgc3RhcnRQYXRoIGVuZFBhdGhcblxuXG50eXBlIE1zZ1xuICAgID0gRXhpc3RhbmNlQ2hlY2tlZCAoUmVzdWx0IEZpbGVTeXN0ZW0uRXJyb3IgUGF0aClcbiAgICB8IENvbXBpbGVyRG93bmxvYWRlZCAoUmVzdWx0IChIdHRwQ2xpZW50LkVycm9yIEJ5dGVzKSAoSHR0cENsaWVudC5SZXNwb25zZSBCeXRlcykpXG4gICAgfCBDb21waWxlckluc3RhbGxlZCAoUmVzdWx0IEZpbGVTeXN0ZW0uRXJyb3Ige30pXG5cblxudXBkYXRlIDogTXNnIC0+IE1vZGVsIC0+IHsgbW9kZWwgOiBNb2RlbCwgY29tbWFuZCA6IENtZCBNc2cgfVxudXBkYXRlIG1zZyBtb2RlbCA9XG4gICAgY2FzZSBtc2cgb2ZcbiAgICAgICAgRXhpc3RhbmNlQ2hlY2tlZCAoRXJyIF8pIC0+XG4gICAgICAgICAgICB7IG1vZGVsID0gbW9kZWxcbiAgICAgICAgICAgICwgY29tbWFuZCA9XG4gICAgICAgICAgICAgICAgY2FzZSBtb2RlbC5yZW1vdGVQYXRoIG9mXG4gICAgICAgICAgICAgICAgICAgIEp1c3QgcmVtb3RlUGF0aCAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgU3RyZWFtLnNlbmRMaW5lIG1vZGVsLnN0ZG91dCAoXCJDb21waWxlciBub3QgZm91bmQgYXQgXCIgKysgbW9kZWwucGF0aFRvU3RyaW5nIG1vZGVsLmxvY2FsUGF0aCArKyBcIi4gRG93bmxvYWRpbmcuLi5cIilcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB8PiBUYXNrLmFuZFRoZW4gKFxce30gLT4gZG93bmxvYWRCaW5hcnkgbW9kZWwuaHR0cFBlcm1pc3Npb24gcmVtb3RlUGF0aClcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB8PiBUYXNrLmF0dGVtcHQgQ29tcGlsZXJEb3dubG9hZGVkXG4gICAgICAgICAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICBTdHJlYW0uc2VuZExpbmUgbW9kZWwuc3RkZXJyIChcIkNvbXBpbGVyIG5vdCBmb3VuZCBhdCBcIiArKyBtb2RlbC5wYXRoVG9TdHJpbmcgbW9kZWwubG9jYWxQYXRoKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHw+IFRhc2suZXhlY3V0ZVxuICAgICAgICAgICAgfVxuXG4gICAgICAgIEV4aXN0YW5jZUNoZWNrZWQgKE9rIF8pIC0+XG4gICAgICAgICAgICB7IG1vZGVsID0gbW9kZWxcbiAgICAgICAgICAgICwgY29tbWFuZCA9XG4gICAgICAgICAgICAgICAgcnVuQ29tcGlsZXIgbW9kZWxcbiAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5leGVjdXRlXG4gICAgICAgICAgICB9XG5cbiAgICAgICAgQ29tcGlsZXJEb3dubG9hZGVkIChFcnIgKChIdHRwQ2xpZW50LkJhZFN0YXR1cyByZXMpIGFzIGVycikpIC0+XG4gICAgICAgICAgICBpZiByZXMuc3RhdHVzQ29kZSA9PSAzMDIgdGhlblxuICAgICAgICAgICAgICAgIGNhc2UgRGljdC5nZXQgXCJsb2NhdGlvblwiIHJlcy5oZWFkZXJzIG9mXG4gICAgICAgICAgICAgICAgICAgIEp1c3QgWyBsb2NhdGlvbiBdIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICB7IG1vZGVsID0gbW9kZWxcbiAgICAgICAgICAgICAgICAgICAgICAgICwgY29tbWFuZCA9XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgZG93bmxvYWRCaW5hcnkgbW9kZWwuaHR0cFBlcm1pc3Npb24gbG9jYXRpb25cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5hdHRlbXB0IENvbXBpbGVyRG93bmxvYWRlZFxuICAgICAgICAgICAgICAgICAgICAgICAgfVxuXG4gICAgICAgICAgICAgICAgICAgIF8gLT5cbiAgICAgICAgICAgICAgICAgICAgICAgIHsgbW9kZWwgPSBtb2RlbFxuICAgICAgICAgICAgICAgICAgICAgICAgLCBjb21tYW5kID1cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBTdHJlYW0uc2VuZExpbmUgbW9kZWwuc3RkZXJyIFwiTWlzc2luZywgb3IgdmFndWUsICdsb2NhdGlvbicgaGVhZGVyIGluIDMwMiByZXNwb25zZSBmcm9tIHNlcnZlci5cIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB8PiBUYXNrLmV4ZWN1dGVcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgZWxzZVxuICAgICAgICAgICAgICAgIHsgbW9kZWwgPSBtb2RlbFxuICAgICAgICAgICAgICAgICwgY29tbWFuZCA9XG4gICAgICAgICAgICAgICAgICAgIFN0cmVhbS5zZW5kTGluZSBtb2RlbC5zdGRlcnIgKEh0dHBDbGllbnQuZXJyb3JUb1N0cmluZyBlcnIpXG4gICAgICAgICAgICAgICAgICAgICAgICB8PiBUYXNrLmV4ZWN1dGVcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgIFxuICAgICAgICBDb21waWxlckRvd25sb2FkZWQgKEVyciBlcnIpIC0+XG4gICAgICAgICAgICAgICAgeyBtb2RlbCA9IG1vZGVsXG4gICAgICAgICAgICAgICAgLCBjb21tYW5kID1cbiAgICAgICAgICAgICAgICAgICAgU3RyZWFtLnNlbmRMaW5lIG1vZGVsLnN0ZGVyciAoSHR0cENsaWVudC5lcnJvclRvU3RyaW5nIGVycilcbiAgICAgICAgICAgICAgICAgICAgICAgIHw+IFRhc2suZXhlY3V0ZVxuICAgICAgICAgICAgICAgIH1cbiAgICAgICAgXG4gICAgICAgIENvbXBpbGVyRG93bmxvYWRlZCAoT2sgcmVzKSAtPlxuICAgICAgICAgICAgbGV0XG4gICAgICAgICAgICAgICAgY2FjaGVGb2xkZXIgPVxuICAgICAgICAgICAgICAgICAgICBQYXRoLnBhcmVudFBhdGggbW9kZWwubG9jYWxQYXRoXG4gICAgICAgICAgICAgICAgICAgICAgICB8PiBNYXliZS53aXRoRGVmYXVsdCBQYXRoLmVtcHR5XG4gICAgICAgICAgICBpblxuICAgICAgICAgICAgeyBtb2RlbCA9IG1vZGVsXG4gICAgICAgICAgICAsIGNvbW1hbmQgPVxuICAgICAgICAgICAgICAgIEZpbGVTeXN0ZW0ubWFrZURpcmVjdG9yeSBtb2RlbC5mc1Blcm1pc3Npb24geyByZWN1cnNpdmUgPSBUcnVlIH0gY2FjaGVGb2xkZXJcbiAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5hbmRUaGVuIChcXF9jYWNoZUZvbGRlciAtPiBGaWxlU3lzdGVtLndyaXRlRmlsZSBtb2RlbC5mc1Blcm1pc3Npb24gcmVzLmRhdGEgbW9kZWwubG9jYWxQYXRoKVxuICAgICAgICAgICAgICAgICAgICB8PiBUYXNrLmFuZFRoZW4gXG4gICAgICAgICAgICAgICAgICAgICAgICAoRmlsZVN5c3RlbS5jaGFuZ2VBY2Nlc3NcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBtb2RlbC5mc1Blcm1pc3Npb25cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB7IG93bmVyID0gWyBGaWxlU3lzdGVtLlJlYWQsIEZpbGVTeXN0ZW0uV3JpdGUsIEZpbGVTeXN0ZW0uRXhlY3V0ZSBdXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBncm91cCA9IFsgRmlsZVN5c3RlbS5SZWFkLCBGaWxlU3lzdGVtLkV4ZWN1dGUgXVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICwgb3RoZXJzID0gWyBGaWxlU3lzdGVtLlJlYWQsIEZpbGVTeXN0ZW0uRXhlY3V0ZSBdXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgKVxuICAgICAgICAgICAgICAgICAgICB8PiBUYXNrLmFuZFRoZW4gKFxcX2JpblBhdGggLT4gU3RyZWFtLnNlbmRMaW5lIG1vZGVsLnN0ZG91dCBcIkRvd25sb2FkZWRcIilcbiAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5hdHRlbXB0IENvbXBpbGVySW5zdGFsbGVkXG4gICAgICAgICAgICB9XG4gICAgICAgIFxuICAgICAgICBDb21waWxlckluc3RhbGxlZCAoRXJyIGZzRXJyKSAtPlxuICAgICAgICAgICAgeyBtb2RlbCA9IG1vZGVsXG4gICAgICAgICAgICAsIGNvbW1hbmQgPVxuICAgICAgICAgICAgICAgIFN0cmVhbS5zZW5kTGluZSBtb2RlbC5zdGRlcnIgKFwiRmFpbGVkIHRvIGluc3RhbGwgYmluYXJ5IGFmdGVyIGRvd25sb2FkLCBkdWUgdG8gZXJyb3I6IFwiICsrIEZpbGVTeXN0ZW0uZXJyb3JUb1N0cmluZyBmc0VycilcbiAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5leGVjdXRlXG4gICAgICAgICAgICB9XG4gICAgICAgIFxuICAgICAgICBDb21waWxlckluc3RhbGxlZCAoT2sge30pIC0+XG4gICAgICAgICAgICB7IG1vZGVsID0gbW9kZWxcbiAgICAgICAgICAgICwgY29tbWFuZCA9XG4gICAgICAgICAgICAgICAgcnVuQ29tcGlsZXIgbW9kZWxcbiAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5leGVjdXRlXG4gICAgICAgICAgICB9XG5cblxuZG93bmxvYWRCaW5hcnkgOiBIdHRwQ2xpZW50LlBlcm1pc3Npb24gLT4gU3RyaW5nIC0+IFRhc2sgKEh0dHBDbGllbnQuRXJyb3IgQnl0ZXMpIChIdHRwQ2xpZW50LlJlc3BvbnNlIEJ5dGVzKVxuZG93bmxvYWRCaW5hcnkgcGVybWlzc2lvbiB1cmwgPVxuICAgIEh0dHBDbGllbnQuZ2V0IHVybFxuICAgICAgICB8PiBIdHRwQ2xpZW50LmV4cGVjdEJ5dGVzXG4gICAgICAgIHw+IEh0dHBDbGllbnQuc2VuZCBwZXJtaXNzaW9uXG5cblxucnVuQ29tcGlsZXIgOiBNb2RlbCAtPiBUYXNrIHggUHJvY2Vzcy5JZFxucnVuQ29tcGlsZXIgbW9kZWwgPVxuICAgIGxldFxuICAgICAgICBjb2xvckVudlZhciA9XG4gICAgICAgICAgICBpZiBtb2RlbC51c2VDb2xvciB0aGVuXG4gICAgICAgICAgICAgICAgRGljdC5zaW5nbGV0b24gXCJGT1JDRV9DT0xPUlwiIFwiMVwiXG4gICAgICAgICAgICBlbHNlXG4gICAgICAgICAgICAgICAgRGljdC5zaW5nbGV0b24gXCJOT19DT0xPUlwiIFwiMVwiXG4gICAgICBpblxuICAgIENoaWxkUHJvY2Vzcy5zcGF3biBtb2RlbC5jcFBlcm1pc3Npb24gKG1vZGVsLnBhdGhUb1N0cmluZyBtb2RlbC5sb2NhbFBhdGgpIG1vZGVsLmFyZ3MgPHxcbiAgICAgICAgeyBDaGlsZFByb2Nlc3MuZGVmYXVsdFNwYXduT3B0aW9uc1xuICAgICAgICAgICAgfCBlbnZpcm9ubWVudFZhcmlhYmxlcyA9IFxuICAgICAgICAgICAgICAgIENoaWxkUHJvY2Vzcy5NZXJnZVdpdGhFbnZpcm9ubWVudFZhcmlhYmxlcyBjb2xvckVudlZhclxuICAgICAgICB9XG4iLAogICAgICAgICJtb2R1bGUgRmlsZVN5c3RlbS5QYXRoIGV4cG9zaW5nIFxuICAgICggUGF0aFxuICAgIC0tXG4gICAgLCBlbXB0eVxuICAgICwgZnJvbVBvc2l4U3RyaW5nXG4gICAgLCB0b1Bvc2l4U3RyaW5nXG4gICAgLCBmcm9tV2luMzJTdHJpbmdcbiAgICAsIHRvV2luMzJTdHJpbmdcbiAgICAtLVxuICAgICwgZmlsZW5hbWVXaXRoRXh0ZW5zaW9uXG4gICAgLCBwYXJlbnRQYXRoXG4gICAgLS1cbiAgICAsIGFwcGVuZFxuICAgICwgcHJlcGVuZFxuICAgICwgam9pblxuICAgIClcblxuXG57LXwgQSBwYXRoIHJlcHJlc2VudHMgdGhlIGxvY2F0aW9uIG9mIGEgZmlsZSBvciBkaXJlY3RvcnkgaW4gYSBmaWxlc3lzdGVtLlxuXG5AZG9jcyBQYXRoXG5cbiMjIENvbnN0cnVjdG9yc1xuXG5AZG9jcyBlbXB0eSwgZnJvbVBvc2l4U3RyaW5nLCB0b1Bvc2l4U3RyaW5nLCBmcm9tV2luMzJTdHJpbmcsIHRvV2luMzJTdHJpbmdcblxuIyMgUXVlcnlcblxuQGRvY3MgZmlsZW5hbWVXaXRoRXh0ZW5zaW9uLCBwYXJlbnRQYXRoXG5cbiMjIE1hbmlwdWxhdGlvblxuXG5AZG9jcyBhcHBlbmQsIHByZXBlbmQsIGpvaW5cbi19XG5cblxuaW1wb3J0IFRhc2sgZXhwb3NpbmcgKFRhc2spXG5pbXBvcnQgR3Jlbi5LZXJuZWwuRmlsZVBhdGhcblxuXG57LXwgQSBjcm9zcy1wbGF0Zm9ybSByZXByZXNlbnRhdGlvbiBvZiBhIGZpbGVzeXN0ZW0gcGF0aC5cblxuSWYgYHJvb3RgIGlzIGVtcHR5LCBpdCBtZWFucyB0aGF0IHRoZSBwYXRoIGlzIHJlbGF0aXZlIHRvIHRoZSB3b3JraW5nIGRpcmVjdG9yeS5cbk9uIHBvc2l4LWNvbXBhdGlibGUgc3lzdGVtcyAoTGludXgsIE1hYy4uLiksIHRoZSByb290IHZhbHVlIGlzIFwiL1wiIGlmIG5vdCBlbXB0eS5cbk9uIFdpbmRvd3MsIHRoZSByb290IHJlZmVycyB0byB0aGUgc3BlY2lmaWMgZGlzayB0aGF0IHRoZSBwYXRoIGFwcGxpZXMgdG8uXG5cbmBmaWxlbmFtZWAgKGFuZCBgZXh0ZW5zaW9uYCkgcmVmZXJzIHRvIHRoZSBsYXN0IHBhcnQgb2YgYSBwYXRoLiBJdCBjYW4gc3RpbGxcbnJlcHJlc2VudCBhIGRpcmVjdG9yeS5cblxuLX1cbnR5cGUgYWxpYXMgUGF0aCA9XG4gICAgeyByb290IDogU3RyaW5nXG4gICAgLCBkaXJlY3RvcnkgOiBBcnJheSBTdHJpbmdcbiAgICAsIGZpbGVuYW1lIDogU3RyaW5nXG4gICAgLCBleHRlbnNpb24gOiBTdHJpbmdcbiAgICB9XG5cblxuey18IFRoZSBlbXB0eSBbUGF0aF0oI1BhdGgpLiBOb3JtYWxseSB0cmVhdGVkIGFzIHRoZSBjdXJyZW50IGRpcmVjdG9yeS5cbi19XG5lbXB0eSA6IFBhdGhcbmVtcHR5ID1cbiAgICB7IHJvb3QgPSBcIlwiXG4gICAgLCBkaXJlY3RvcnkgPSBbXVxuICAgICwgZmlsZW5hbWUgPSBcIlwiXG4gICAgLCBleHRlbnNpb24gPSBcIlwiXG4gICAgfVxuXG5cbnstfCBCdWlsZCBhIFtQYXRoXSgjUGF0aCkgZnJvbSBhIGBTdHJpbmdgLiBUaGUgYFN0cmluZ2Agc2hvdWxkIHJlcHJlc2VudCBhIFBvc2l4LWNvbXBhdGlibGUgcGF0aC5cbi19XG5mcm9tUG9zaXhTdHJpbmcgOiBTdHJpbmcgLT4gUGF0aFxuZnJvbVBvc2l4U3RyaW5nID1cbiAgICBHcmVuLktlcm5lbC5GaWxlUGF0aC5mcm9tUG9zaXhcblxuXG57LXwgU3RyaW5nIHJlcHJlc2VudGF0aW9uIG9mIGEgW1BhdGhdKCNQYXRoKSBmb3IgUG9zaXggc3lzdGVtcy5cbi19XG50b1Bvc2l4U3RyaW5nIDogUGF0aCAtPiBTdHJpbmdcbnRvUG9zaXhTdHJpbmcgPVxuICAgIEdyZW4uS2VybmVsLkZpbGVQYXRoLnRvUG9zaXhcblxuXG57LXwgQnVpbGQgYSBbUGF0aF0oI1BhdGgpIGZyb20gYSBgU3RyaW5nYC4gVGhlIGBTdHJpbmdgIHNob3VsZCByZXByZXNlbnQgYSBXaW5kb3dzLWNvbXBhdGlibGUgcGF0aC5cbi19XG5mcm9tV2luMzJTdHJpbmcgOiBTdHJpbmcgLT4gUGF0aFxuZnJvbVdpbjMyU3RyaW5nID1cbiAgICBHcmVuLktlcm5lbC5GaWxlUGF0aC5mcm9tV2luMzJcblxuXG57LXwgYFN0cmluZ2AgcmVwcmVzZW50YXRpb24gb2YgYSBbUGF0aF0oI1BhdGgpIGZvciBXaW5kb3dzLlxuLX1cbnRvV2luMzJTdHJpbmcgOiBQYXRoIC0+IFN0cmluZ1xudG9XaW4zMlN0cmluZyA9XG4gICAgR3Jlbi5LZXJuZWwuRmlsZVBhdGgudG9XaW4zMlxuXG5cbnstfCBSZXR1cm4gdGhlIGZpbGVuYW1lIGFuZCBmaWxlIGV4dGVuc2lvbiBmb3IgYSBbUGF0aF0oI1BhdGgpLlxuXG4gICAgXCIvaG9tZS9tZS9maWxlLm1kXCJcbiAgICAgICAgfD4gZnJvbVBvc2l4U3RyaW5nXG4gICAgICAgIHw+IGZpbGVuYW1lV2l0aEV4dGVuc2lvblxuICAgICAgICAtLSByZXR1cm5zIFwiZmlsZS5tZFwiXG4tfVxuZmlsZW5hbWVXaXRoRXh0ZW5zaW9uIDogUGF0aCAtPiBTdHJpbmdcbmZpbGVuYW1lV2l0aEV4dGVuc2lvbiBwYXRoID1cbiAgICBpZiBTdHJpbmcuaXNFbXB0eSBwYXRoLmV4dGVuc2lvbiB0aGVuXG4gICAgICAgIHBhdGguZmlsZW5hbWVcblxuICAgIGVsc2VcbiAgICAgICAgcGF0aC5maWxlbmFtZSArKyBcIi5cIiArKyBwYXRoLmV4dGVuc2lvblxuXG5cbnstfCBSZXR1cm4gYSBbUGF0aF0oI1BhdGgpIHRoYXQgcmVwcmVzZW50cyB0aGUgZGlyZWN0b3J5IHdoaWNoIGhvbGRzIHRoZSBnaXZlbiBbUGF0aF0oI1BhdGgpXG5cbiAgICBcIi9ob21lL21lL2ZpbGUubWRcIlxuICAgICAgICB8PiBmcm9tUG9zaXhTdHJpbmdcbiAgICAgICAgfD4gcGFyZW50UGF0aFxuICAgICAgICAtLSByZXR1cm5zIChKdXN0IFwiL2hvbWUvbWVcIilcbi19XG5wYXJlbnRQYXRoIDogUGF0aCAtPiBNYXliZSBQYXRoXG5wYXJlbnRQYXRoIHBhdGggPVxuICAgIGNhc2UgQXJyYXkucG9wTGFzdCBwYXRoLmRpcmVjdG9yeSBvZlxuICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICBpZiBmaWxlbmFtZVdpdGhFeHRlbnNpb24gcGF0aCA9PSBcIlwiIHRoZW5cbiAgICAgICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgICAgIGVsc2VcbiAgICAgICAgICAgICAgICBKdXN0XG4gICAgICAgICAgICAgICAgICAgIHsgcGF0aFxuICAgICAgICAgICAgICAgICAgICAgICAgfCBmaWxlbmFtZSA9IFwiXCJcbiAgICAgICAgICAgICAgICAgICAgICAgICwgZXh0ZW5zaW9uID0gXCJcIlxuICAgICAgICAgICAgICAgICAgICB9XG5cbiAgICAgICAgSnVzdCB7IGxhc3QsIGluaXRpYWwgfSAtPlxuICAgICAgICAgICAgbGV0XG4gICAgICAgICAgICAgICAgeyBmaWxlbmFtZSwgZXh0ZW5zaW9uIH0gPVxuICAgICAgICAgICAgICAgICAgICBjYXNlIFN0cmluZy5zcGxpdCBcIi5cIiBsYXN0IG9mXG4gICAgICAgICAgICAgICAgICAgICAgICBbIGZpbGUsIGV4dCBdIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgeyBmaWxlbmFtZSA9IGZpbGVcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAsIGV4dGVuc2lvbiA9IGV4dFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgICAgICAgICAgICAgXyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHsgZmlsZW5hbWUgPSBsYXN0XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgLCBleHRlbnNpb24gPSBcIlwiXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgaW5cbiAgICAgICAgICAgIEp1c3RcbiAgICAgICAgICAgICAgICB7IHBhdGhcbiAgICAgICAgICAgICAgICAgICAgfCBkaXJlY3RvcnkgPSBpbml0aWFsXG4gICAgICAgICAgICAgICAgICAgICwgZmlsZW5hbWUgPSBmaWxlbmFtZVxuICAgICAgICAgICAgICAgICAgICAsIGV4dGVuc2lvbiA9IGV4dGVuc2lvblxuICAgICAgICAgICAgICAgIH1cblxuXG57LXwgSm9pbiB0d28gcGF0aHMgYnkgYXBwZW5kaW5nIHRoZSBmaXJzdCBbUGF0aF0oI1BhdGgpIG9udG8gdGhlIHNlY29uZC5cbi19XG5hcHBlbmQgOiBQYXRoIC0+IFBhdGggLT4gUGF0aFxuYXBwZW5kIGxlZnQgcmlnaHQgPVxuICAgIHByZXBlbmQgcmlnaHQgbGVmdFxuXG5cbnstfCBKb2luIHR3byBwYXRocyBieSBwcmVwZW5kaW5nIHRoZSBmaXJzdCBbUGF0aF0oI1BhdGgpIG9udG8gdGhlIHNlY29uZC5cbi19XG5wcmVwZW5kIDogUGF0aCAtPiBQYXRoIC0+IFBhdGhcbnByZXBlbmQgbGVmdCByaWdodCA9XG4gICAgeyBsZWZ0XG4gICAgICAgIHwgZGlyZWN0b3J5ID1cbiAgICAgICAgICAgIGxlZnQuZGlyZWN0b3J5XG4gICAgICAgICAgICAgICAgfD4gQXJyYXkucHVzaExhc3QgKGZpbGVuYW1lV2l0aEV4dGVuc2lvbiBsZWZ0KVxuICAgICAgICAgICAgICAgIHw+IEFycmF5LmFwcGVuZCByaWdodC5kaXJlY3RvcnlcbiAgICAgICAgICAgICAgICB8PiBBcnJheS5maWx0ZXIgKFxcZGlyIC0+IGRpciAvPSBcIlwiKVxuICAgICAgICAsIGZpbGVuYW1lID0gcmlnaHQuZmlsZW5hbWVcbiAgICAgICAgLCBleHRlbnNpb24gPSByaWdodC5leHRlbnNpb25cbiAgICB9XG5cblxuey18IEpvaW4gYWxsIHBhdGhzIGluIGFuIGBBcnJheWAuXG4tfVxuam9pbiA6IEFycmF5IFBhdGggLT4gUGF0aFxuam9pbiBwYXRocyA9XG4gICAgQXJyYXkuZm9sZGwgYXBwZW5kIChmcm9tUG9zaXhTdHJpbmcgXCIuXCIpIHBhdGhzXG4iLAogICAgICAgICJtb2R1bGUgQ2hpbGRQcm9jZXNzIGV4cG9zaW5nIFxuICAgICggUGVybWlzc2lvblxuICAgICwgaW5pdGlhbGl6ZVxuICAgIC0tXG4gICAgLCBSdW5PcHRpb25zXG4gICAgLCBkZWZhdWx0UnVuT3B0aW9uc1xuICAgICwgU2hlbGwoLi4pXG4gICAgLCBXb3JraW5nRGlyZWN0b3J5KC4uKVxuICAgICwgRW52aXJvbm1lbnRWYXJpYWJsZXMoLi4pXG4gICAgLCBSdW5EdXJhdGlvbiguLilcbiAgICAtLVxuICAgICwgRmFpbGVkUnVuXG4gICAgLCBTdWNjZXNzZnVsUnVuXG4gICAgLCBydW5cbiAgICAsIHJ1bldpdGhEZWZhdWx0T3B0aW9uc1xuICAgIC0tXG4gICAgLCBTcGF3bk9wdGlvbnNcbiAgICAsIENvbm5lY3Rpb24oLi4pXG4gICAgLCBkZWZhdWx0U3Bhd25PcHRpb25zXG4gICAgLCBzcGF3blxuICAgICwgc3Bhd25XaXRoRGVmYXVsdE9wdGlvbnNcbiAgICApXG5cblxuey18IEEgcnVubmluZyBwcm9ncmFtIGlzIGEgcHJvY2Vzcy4gQSBwcm9jZXNzIHNwYXduZWQgZnJvbSBhbm90aGVyIHByb2Nlc3MgaXMga25vd24gYXMgYSBjaGlsZCBwcm9jZXNzLlxuXG5UaGlzIG1vZHVsZSBhbGxvdyB5b3UgdG8gc3Bhd24gY2hpbGQgcHJvY2Vzc2VzLlxuXG4jIyBJbml0aWFsaXphdGlvblxuXG5AZG9jcyBQZXJtaXNzaW9uLCBpbml0aWFsaXplXG5cbiMjIFJ1bm5pbmcgcHJvY2Vzc2VzXG5cbkBkb2NzIFJ1bk9wdGlvbnMsIGRlZmF1bHRSdW5PcHRpb25zLCBTaGVsbCwgV29ya2luZ0RpcmVjdG9yeSwgRW52aXJvbm1lbnRWYXJpYWJsZXMsIFJ1bkR1cmF0aW9uXG5AZG9jcyBGYWlsZWRSdW4sIFN1Y2Nlc3NmdWxSdW4sIHJ1biwgcnVuV2l0aERlZmF1bHRPcHRpb25zXG5cbiMjIFNwYXduaW5nIHByb2Nlc3Nlc1xuXG5AZG9jcyBTcGF3bk9wdGlvbnMsIENvbm5lY3Rpb24sIGRlZmF1bHRTcGF3bk9wdGlvbnMsIHNwYXduLCBzcGF3bldpdGhEZWZhdWx0T3B0aW9uc1xuLX1cblxuXG5pbXBvcnQgR3Jlbi5LZXJuZWwuQ2hpbGRQcm9jZXNzXG5pbXBvcnQgQnl0ZXMgZXhwb3NpbmcgKEJ5dGVzKVxuaW1wb3J0IERpY3QgZXhwb3NpbmcgKERpY3QpXG5pbXBvcnQgVGFzayBleHBvc2luZyAoVGFzaylcbmltcG9ydCBJbml0XG5pbXBvcnQgSW50ZXJuYWwuSW5pdFxuaW1wb3J0IFByb2Nlc3NcblxuXG57LXwgVGhpcyB2YWx1ZSByZXByZXNlbnRzIHRoZSBwZXJtaXNzaW9uIHRvIHNwYXduIGNoaWxkIHByb2Nlc3Nlcy5cblxuT25seSBjb2RlIHlvdSB0cnVzdCBzaG91bGQgaGF2ZSBhY2Nlc3MgdG8gdGhpcyB2YWx1ZS5cbi19XG50eXBlIFBlcm1pc3Npb25cbiAgICA9IFBlcm1pc3Npb25cblxuXG57LXwgSW5pdGlhbGl6ZSB0aGUgYENoaWxkUHJvY2Vzc2Agc3Vic3lzdGVtLCB3aGljaCBnYWlucyB5b3UgdGhlIHBlcm1pc3Npb24gdG9cbnNwYXduIGNoaWxkIHByb2Nlc3Nlcy5cbi19XG5pbml0aWFsaXplIDogSW5pdC5UYXNrIFBlcm1pc3Npb25cbmluaXRpYWxpemUgPVxuICAgIFRhc2suc3VjY2VlZCBQZXJtaXNzaW9uXG4gICAgICAgIHw+IEludGVybmFsLkluaXQuVGFza1xuXG5cbi0tIE9QVElPTlNcblxuXG57LXwgT3B0aW9ucyB0byBjdXN0b21pemUgdGhlIGV4ZWN1dGlvbiBvZiBhIGNoaWxkIHByb2Nlc3MgY3JlYXRlZCB3aXRoIFtydW5dKCNydW4pLlxuXG4qIGBzaGVsbGAgaXMgdGhlIHNoZWxsIHRvIHJ1biB0aGUgcHJvY2VzcyBpbiAoaWYgYW55KVxuKiBgd29ya2luZ0RpcmVjdG9yeWAgc3BlY2lmaWVzIHRoZSB3b3JraW5nIGRpcmVjdG9yeSBvZiB0aGUgcHJvY2Vzc1xuKiBgZW52aXJvbm1lbnRWYXJpYWJsZXNgIHNwZWNpZmllcyB0aGUgZW52aXJvbm1lbnQgdmFyaWFibGVzIHRoZSBwcm9jZXNzIGhhcyBhY2Nlc3MgdG9cbiogYG1heGltdW1CeXRlc1dyaXR0ZW5Ub1N0cmVhbXNgIHNwZWNpZmllcyBhbiB1cHBlciBib3VuZCBvZiBieXRlcyB0aGF0IGNhbiBiZSByZXR1cm5lZCBmcm9tIHRoZSBwcm9jZXNzXG4qIGBydW5EdXJhdGlvbmAgc3BlY2lmaWVzIGEgbWF4aW11bSBhbW91bnQgb2YgdGltZSBhIHByb2Nlc3MgaXMgYWxsb3dlZCB0byBydW4gYmVmb3JlIGV4aXRpbmdcbi19XG50eXBlIGFsaWFzIFJ1bk9wdGlvbnMgPVxuICAgIHsgc2hlbGwgOiBTaGVsbFxuICAgICwgd29ya2luZ0RpcmVjdG9yeSA6IFdvcmtpbmdEaXJlY3RvcnlcbiAgICAsIGVudmlyb25tZW50VmFyaWFibGVzIDogRW52aXJvbm1lbnRWYXJpYWJsZXNcbiAgICAsIG1heGltdW1CeXRlc1dyaXR0ZW5Ub1N0cmVhbXMgOiBJbnRcbiAgICAsIHJ1bkR1cmF0aW9uIDogUnVuRHVyYXRpb25cbiAgICB9XG5cblxuey18IEEgbmljZSBkZWZhdWx0IHNldCBvZiBvcHRpb25zIGZvciB0aGUgW3J1bl0oI3J1bikgZnVuY3Rpb25cbi19XG5kZWZhdWx0UnVuT3B0aW9ucyA6IFJ1bk9wdGlvbnNcbmRlZmF1bHRSdW5PcHRpb25zID1cbiAgICB7IHNoZWxsID0gRGVmYXVsdFNoZWxsXG4gICAgLCB3b3JraW5nRGlyZWN0b3J5ID0gSW5oZXJpdFdvcmtpbmdEaXJlY3RvcnlcbiAgICAsIGVudmlyb25tZW50VmFyaWFibGVzID0gSW5oZXJpdEVudmlyb25tZW50VmFyaWFibGVzXG4gICAgLCBtYXhpbXVtQnl0ZXNXcml0dGVuVG9TdHJlYW1zID0gMTAyNCAqIDEwMjQgLS0gMU1iXG4gICAgLCBydW5EdXJhdGlvbiA9IE5vTGltaXRcbiAgICB9XG5cblxuey18IFdoaWNoIHNoZWxsIHNob3VsZCB0aGUgY2hpbGQgcHJvY2VzcyBydW4gaW4/XG5cbiogYE5vU2hlbGxgIGV4ZWN1dGVzIHRoZSBwcm9jZXNzIGRpcmVjdGx5LCB3aXRob3V0IGFueSBzaGVsbC4gQSBsaXR0bGUgYml0IG1vcmUgZWZmaWNpZW50LCBidXQgeW91IGxvc2Ugc29tZSBjb252aW5pZW5jZSBhcyBzaGVsbCBiZWhhdmlvdXIgKGxpa2UgZ2xvYiBwYXR0ZXJucykgaXNuJ3QgYXZhaWxhYmxlIGZvciBhcmd1bWVudHNcbiogYERlZmF1bHRTaGVsbGAgZXhlY3V0ZXMgdGhlIHByb2Nlc3MgaW4gdGhlIGRlZmF1bHQgc2hlbGwgZm9yIHRoZSBjdXJyZW50bHkgcnVubmluZyBzeXN0ZW1cbiogYEN1c3RvbVNoZWxsYCBleGVjdXRlcyB0aGUgcHJvY2VzcyBpbiB0aGUgc3BlY2lmaWVkIHNoZWxsLlxuLX1cbnR5cGUgU2hlbGxcbiAgICA9IE5vU2hlbGxcbiAgICB8IERlZmF1bHRTaGVsbFxuICAgIHwgQ3VzdG9tU2hlbGwgU3RyaW5nXG5cblxuey18IFdoYXQgc2hvdWxkIGJlIHRoZSB3b3JraW5nIGRpcmVjdG9yeSBvZiB0aGUgcHJvY2Vzcz9cblxuKiBgSW5oZXJpdFdvcmtpbmdEaXJlY3RvcnlgIGluaGVyaXRzIHRoZSB3b3JraW5nIGRpcmVjdG9yeSBmcm9tIGl0cyBwYXJlbnRcbiogYFNldFdvcmtpbmdEaXJlY3RvcnlgIHNldHMgdGhlIHdvcmtpbmcgZGlyZWN0b3J5IHRvIHRoZSBzcGVjaWZpZWQgdmFsdWUgKGRvZXNuJ3QgYWZmZWN0IHBhcmVudClcbi19XG50eXBlIFdvcmtpbmdEaXJlY3RvcnlcbiAgICA9IEluaGVyaXRXb3JraW5nRGlyZWN0b3J5XG4gICAgfCBTZXRXb3JraW5nRGlyZWN0b3J5IFN0cmluZ1xuXG5cbnstfCBXaGF0IHNob3VsZCBiZSB0aGUgZW52aXJvbm1lbnQgdmFyaWFibGVzIG9mIHRoZSBwcm9jZXNzP1xuXG4qIGBJbmhlcml0RW52aXJvbm1lbnRWYXJpYWJsZXNgIGluaGVyaXRzIHRoZSBlbnZpcm9ubWVudCB2YXJpYWJsZXMgZnJvbSBpdHMgcGFyZW50XG4qIGBNZXJnZVdpdGhFbnZpcm9ubWVudFZhcmlhYmxlc2AgaW5oZXJpdHMgdGhlIGVudmlyb25tZW50IHZhcmlhYmxlcyBmcm9tIGl0cyBwYXJlbnQsIHdpdGggdGhlIHNwZWNpZmllZCBtb2RpZmljYXRpb25zXG4qIGBSZXBsYWNlRW52aXJvbm1lbnRWYXJpYWJsZXNgIHNldHMgdGhlIGVudmlyb25tZW50IHZhcmlhYmxlcyB0byB0aGUgc3BlY2lmaWVkIGRpY3Rpb25hcnlcbi19XG50eXBlIEVudmlyb25tZW50VmFyaWFibGVzXG4gICAgPSBJbmhlcml0RW52aXJvbm1lbnRWYXJpYWJsZXNcbiAgICB8IE1lcmdlV2l0aEVudmlyb25tZW50VmFyaWFibGVzIChEaWN0IFN0cmluZyBTdHJpbmcpXG4gICAgfCBSZXBsYWNlRW52aXJvbm1lbnRWYXJpYWJsZXMgKERpY3QgU3RyaW5nIFN0cmluZylcblxuXG57LXwgSG93IGxvbmcgaXMgdGhlIHByb2Nlc3MgYWxsb3dlZCB0byBydW4gYmVmb3JlIGl0J3MgZm9yY2VmdWxseSB0ZXJtaW5hdGVkP1xuXG4qIGBOb0xpbWl0YCBtZWFucyBpdCBjYW4gcnVuIGZvcmV2ZXJcbiogYE1pbGxpc2Vjb25kc2Agc2V0cyB0aGUgbGltaXQgdG8gdGhlIHNwZWNpZmllZCBudW1iZXIgb2YgbWlsbGlzZWNvbmRzXG4tfVxudHlwZSBSdW5EdXJhdGlvblxuICAgID0gTm9MaW1pdFxuICAgIHwgTWlsbGlzZWNvbmRzIEludFxuXG5cbi0tIFJVTlxuXG5cbnstfCBSZXR1cm4gdmFsdWUgd2hlbiBhIHByb2Nlc3MgdGVybWluYXRlcyBkdWUgdG8gYW4gZXJyb3JcblxuVGhlIGV4aXQgY29kZSBwcm92aWRlcyBzb21lIGhpbnQgb2Ygd2hhdCB3ZW50IHdyb25nLCBidXQgd2hhdCBpdCBtZWFucyBkZXBlbmRzIG9uIHRoZSBwcm9ncmFtIHdoaWNoIHdhcyBydW4uXG4tfVxudHlwZSBhbGlhcyBGYWlsZWRSdW4gPVxuICAgIHsgZXhpdENvZGU6IEludFxuICAgICwgc3Rkb3V0IDogQnl0ZXNcbiAgICAsIHN0ZGVyciA6IEJ5dGVzXG4gICAgfVxuXG5cbnstfCBSZXR1cm4gdmFsdWUgd2hlbiBhIHByb2Nlc3MgdGVybWluYXRlcyB3aXRob3V0IGVycm9yXG4tfVxudHlwZSBhbGlhcyBTdWNjZXNzZnVsUnVuID1cbiAgICB7IHN0ZG91dCA6IEJ5dGVzXG4gICAgLCBzdGRlcnIgOiBCeXRlc1xuICAgIH1cblxuXG57LXwgRXhlY3V0ZSBhIHByb2Nlc3Mgd2l0aCBhIGdpdmVuIG5hbWUsIGFyZ3VtZW50cyBhbmQgb3B0aW9ucywgYW5kIHdhaXQgZm9yIGl0IHRvIHRlcm1pbmF0ZS5cblxuICAgIHJ1biBwZXJtaXNzaW9uIFwiY2F0XCIgWyBcIm15X2ZpbGVcIiBdIGRlZmF1bHRSdW5PcHRpb25zXG5cbi19XG5ydW4gOiBQZXJtaXNzaW9uIC0+IFN0cmluZyAtPiBBcnJheSBTdHJpbmcgLT4gUnVuT3B0aW9ucyAtPiBUYXNrIEZhaWxlZFJ1biBTdWNjZXNzZnVsUnVuXG5ydW4gXyBwcm9ncmFtIGFyZ3VtZW50cyBvcHRzID1cbiAgICBHcmVuLktlcm5lbC5DaGlsZFByb2Nlc3MucnVuXG4gICAgICAgIHsgcHJvZ3JhbSA9IHByb2dyYW1cbiAgICAgICAgLCBhcmd1bWVudHMgPSBhcmd1bWVudHNcbiAgICAgICAgLCBzaGVsbCA9XG4gICAgICAgICAgICBjYXNlIG9wdHMuc2hlbGwgb2ZcbiAgICAgICAgICAgICAgICBOb1NoZWxsIC0+XG4gICAgICAgICAgICAgICAgICAgIHsgY2hvaWNlID0gMFxuICAgICAgICAgICAgICAgICAgICAsIHZhbHVlID0gXCJcIlxuICAgICAgICAgICAgICAgICAgICB9XG5cbiAgICAgICAgICAgICAgICBEZWZhdWx0U2hlbGwgLT5cbiAgICAgICAgICAgICAgICAgICAgeyBjaG9pY2UgPSAxXG4gICAgICAgICAgICAgICAgICAgICwgdmFsdWUgPSBcIlwiXG4gICAgICAgICAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgICAgIEN1c3RvbVNoZWxsIHZhbHVlIC0+XG4gICAgICAgICAgICAgICAgICAgIHsgY2hvaWNlID0gMlxuICAgICAgICAgICAgICAgICAgICAsIHZhbHVlID0gdmFsdWVcbiAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAsIHdvcmtpbmdEaXJlY3RvcnkgPVxuICAgICAgICAgICAgIGNhc2Ugb3B0cy53b3JraW5nRGlyZWN0b3J5IG9mXG4gICAgICAgICAgICAgICAgIEluaGVyaXRXb3JraW5nRGlyZWN0b3J5IC0+IFxuICAgICAgICAgICAgICAgICAgICB7IGluaGVyaXQgPSBUcnVlXG4gICAgICAgICAgICAgICAgICAgICwgb3ZlcnJpZGUgPSBcIlwiXG4gICAgICAgICAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgICAgICBTZXRXb3JraW5nRGlyZWN0b3J5IHZhbHVlIC0+XG4gICAgICAgICAgICAgICAgICAgIHsgaW5oZXJpdCA9IEZhbHNlXG4gICAgICAgICAgICAgICAgICAgICwgb3ZlcnJpZGUgPSB2YWx1ZVxuICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICwgZW52aXJvbm1lbnRWYXJpYWJsZXMgPVxuICAgICAgICAgICAgY2FzZSBvcHRzLmVudmlyb25tZW50VmFyaWFibGVzIG9mXG4gICAgICAgICAgICAgICAgSW5oZXJpdEVudmlyb25tZW50VmFyaWFibGVzIC0+IFxuICAgICAgICAgICAgICAgICAgICB7IG9wdGlvbiA9IDBcbiAgICAgICAgICAgICAgICAgICAgLCB2YWx1ZSA9IERpY3QuZW1wdHlcbiAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgIE1lcmdlV2l0aEVudmlyb25tZW50VmFyaWFibGVzIHZhbHVlIC0+XG4gICAgICAgICAgICAgICAgICAgIHsgb3B0aW9uID0gMVxuICAgICAgICAgICAgICAgICAgICAsIHZhbHVlID0gdmFsdWVcbiAgICAgICAgICAgICAgICAgICAgfVxuXG4gICAgICAgICAgICAgICAgUmVwbGFjZUVudmlyb25tZW50VmFyaWFibGVzIHZhbHVlIC0+XG4gICAgICAgICAgICAgICAgICAgIHsgb3B0aW9uID0gMlxuICAgICAgICAgICAgICAgICAgICAsIHZhbHVlID0gdmFsdWVcbiAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAsIG1heGltdW1CeXRlc1dyaXR0ZW5Ub1N0cmVhbXMgPSBvcHRzLm1heGltdW1CeXRlc1dyaXR0ZW5Ub1N0cmVhbXNcbiAgICAgICAgLCBydW5EdXJhdGlvbiA9XG4gICAgICAgICAgICBjYXNlIG9wdHMucnVuRHVyYXRpb24gb2ZcbiAgICAgICAgICAgICAgICBOb0xpbWl0IC0+XG4gICAgICAgICAgICAgICAgICAgIDBcblxuICAgICAgICAgICAgICAgIE1pbGxpc2Vjb25kcyBtcyAtPiBcbiAgICAgICAgICAgICAgICAgICAgbWF4IDAgbXNcbiAgICAgICAgfVxuXG5cbnstfCBTYW1lIGFzIFtydW5dKCNydW4pLCBidXQgd2l0aCBbZGVmYXVsdFJ1bk9wdGlvbnNdKCNkZWZhdWx0UnVuT3B0aW9ucykgcGFzc2VkIGluIGFzIG9wdGlvbnMuXG4tfVxucnVuV2l0aERlZmF1bHRPcHRpb25zIDogUGVybWlzc2lvbiAtPiBTdHJpbmcgLT4gQXJyYXkgU3RyaW5nIC0+IFRhc2sgRmFpbGVkUnVuIFN1Y2Nlc3NmdWxSdW5cbnJ1bldpdGhEZWZhdWx0T3B0aW9ucyBwZXJtaXNzaW9uIHByb2dyYW0gYXJndW1lbnRzID1cbiAgICBydW4gcGVybWlzc2lvbiBwcm9ncmFtIGFyZ3VtZW50cyBkZWZhdWx0UnVuT3B0aW9uc1xuXG5cbi0tIFNQQVdOXG5cblxuey18IE9wdGlvbnMgdG8gY3VzdG9taXplIHRoZSBleGVjdXRpb24gb2YgYSBjaGlsZCBwcm9jZXNzIGNyZWF0ZWQgd2l0aCBbc3Bhd25dKCNzcGF3bikuXG5cbiogYHNoZWxsYCBpcyB0aGUgc2hlbGwgdG8gcnVuIHRoZSBwcm9jZXNzIGluIChpZiBhbnkpXG4qIGB3b3JraW5nRGlyZWN0b3J5YCBzcGVjaWZpZXMgdGhlIHdvcmtpbmcgZGlyZWN0b3J5IG9mIHRoZSBwcm9jZXNzXG4qIGBlbnZpcm9ubWVudFZhcmlhYmxlc2Agc3BlY2lmaWVzIHRoZSBlbnZpcm9ubWVudCB2YXJpYWJsZXMgdGhlIHByb2Nlc3MgaGFzIGFjY2VzcyB0b1xuKiBgcnVuRHVyYXRpb25gIHNwZWNpZmllcyBhIG1heGltdW0gYW1vdW50IG9mIHRpbWUgYSBwcm9jZXNzIGlzIGFsbG93ZWQgdG8gcnVuIGJlZm9yZSBleGl0aW5nXG4qIGBjb25uZWN0aW9uYCBsZXQncyB5b3Ugc3BlY2lmeSBob3cgY2xvc2UgdGhlIG5ldyBwcm9jZXNzIGlzIGNvbm5lY3RlZCB0byB0aGUgYXBwbGljYXRpb25cbi19XG50eXBlIGFsaWFzIFNwYXduT3B0aW9ucyA9XG4gICAgeyBzaGVsbCA6IFNoZWxsXG4gICAgLCB3b3JraW5nRGlyZWN0b3J5IDogV29ya2luZ0RpcmVjdG9yeVxuICAgICwgZW52aXJvbm1lbnRWYXJpYWJsZXMgOiBFbnZpcm9ubWVudFZhcmlhYmxlc1xuICAgICwgcnVuRHVyYXRpb24gOiBSdW5EdXJhdGlvblxuICAgICwgY29ubmVjdGlvbiA6IENvbm5lY3Rpb25cbiAgICB9XG5cblxuey18IFdoYXQgcmVsYXRpb24gc2hvdWxkIHRoZSBuZXdseSBzcGF3bmVkIHByb2Nlc3MgaGF2ZSB3aXRoIHRoZSBydW5uaW5nIGFwcGxpY2F0aW9uP1xuXG4qIGBJbnRlZ3JhdGVkYCBtZWFucyB0aGF0IHRoZSBzcGF3bmVkIHByb2Nlc3Mgc2hhcmVzIHRoZSBzdGRpbiwgc3Rkb3V0IGFuZCBzdGRlcnIgc3RyZWFtcyBhbmQgdGhhdCB0aGUgYXBwbGljYXRpb24gd2lsbCB3YWl0IGZvciBpdHMgdGVybWluYXRpb24uXG4qIGBJZ25vcmVkYCBtZWFucyB0aGF0IHN0ZGluLCBzdGRvdXQgYW5kIHN0ZGVyciBpcyBzZXBlcmF0ZSBidXQgdGhhdCB0aGUgYXBwbGljYXRpb24gd2lsbCBzdGlsbCB3YWl0IGZvciBpdHMgdGVybWluYXRpb24uXG4qIGBEZXRhY2hlZGAgbWVhbnMgdGhhdCB0aGUgYXBwbGljYXRpb24gY2FuIHRlcm1pbmF0ZSBldmVuIGlmIHRoZSBzcGF3bmVkIHByb2Nlc3MgaXMgc3RpbGwgcnVubmluZy5cbi19XG50eXBlIENvbm5lY3Rpb25cbiAgICA9IEludGVncmF0ZWRcbiAgICB8IElnbm9yZWRcbiAgICB8IERldGFjaGVkXG5cblxuey18IEEgbmljZSBkZWZhdWx0IHNldCBvZiBvcHRpb25zIGZvciB0aGUgW3NwYXduXSgjc3Bhd24pIGZ1bmN0aW9uLlxuLX1cbmRlZmF1bHRTcGF3bk9wdGlvbnMgOiBTcGF3bk9wdGlvbnNcbmRlZmF1bHRTcGF3bk9wdGlvbnMgPVxuICAgIHsgc2hlbGwgPSBEZWZhdWx0U2hlbGxcbiAgICAsIHdvcmtpbmdEaXJlY3RvcnkgPSBJbmhlcml0V29ya2luZ0RpcmVjdG9yeVxuICAgICwgZW52aXJvbm1lbnRWYXJpYWJsZXMgPSBJbmhlcml0RW52aXJvbm1lbnRWYXJpYWJsZXNcbiAgICAsIHJ1bkR1cmF0aW9uID0gTm9MaW1pdFxuICAgICwgY29ubmVjdGlvbiA9IEludGVncmF0ZWRcbiAgICB9XG5cblxuey18IFNwYXduIGEgcHJvY2VzcyB3aXRoIGEgZ2l2ZW4gbmFtZSwgYXJndW1lbnRzIGFuZCBvcHRpb25zLCBhbmQgbGV0IGl0IHJ1biBpbiB0aGUgYmFja2dyb3VuZC5cblRoaXMgaXMgbW9zdGx5IGhlbHBmdWwgZm9yIHN0YXJ0aW5nIGxvbmctcnVubmluZyBwcm9jZXNzZXMuXG5cbiAgICBzcGF3biBwZXJtaXNzaW9uIFwidGFpbFwiIFsgXCJteV9maWxlXCIgXSBkZWZhdWx0U3Bhd25PcHRpb25zXG5cbi19XG5zcGF3biA6IFBlcm1pc3Npb24gLT4gU3RyaW5nIC0+IEFycmF5IFN0cmluZyAtPiBTcGF3bk9wdGlvbnMgLT4gVGFzayB4IFByb2Nlc3MuSWRcbnNwYXduIF8gcHJvZ3JhbSBhcmd1bWVudHMgb3B0cyA9XG4gICAgUHJvY2Vzcy5zcGF3biA8fFxuICAgICAgICBHcmVuLktlcm5lbC5DaGlsZFByb2Nlc3Muc3Bhd25cbiAgICAgICAgICAgIHsgcHJvZ3JhbSA9IHByb2dyYW1cbiAgICAgICAgICAgICwgYXJndW1lbnRzID0gYXJndW1lbnRzXG4gICAgICAgICAgICAsIHNoZWxsID1cbiAgICAgICAgICAgICAgICBjYXNlIG9wdHMuc2hlbGwgb2ZcbiAgICAgICAgICAgICAgICAgICAgTm9TaGVsbCAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgeyBjaG9pY2UgPSAwXG4gICAgICAgICAgICAgICAgICAgICAgICAsIHZhbHVlID0gXCJcIlxuICAgICAgICAgICAgICAgICAgICAgICAgfVxuXG4gICAgICAgICAgICAgICAgICAgIERlZmF1bHRTaGVsbCAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgeyBjaG9pY2UgPSAxXG4gICAgICAgICAgICAgICAgICAgICAgICAsIHZhbHVlID0gXCJcIlxuICAgICAgICAgICAgICAgICAgICAgICAgfVxuXG4gICAgICAgICAgICAgICAgICAgIEN1c3RvbVNoZWxsIHZhbHVlIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICB7IGNob2ljZSA9IDJcbiAgICAgICAgICAgICAgICAgICAgICAgICwgdmFsdWUgPSB2YWx1ZVxuICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgLCB3b3JraW5nRGlyZWN0b3J5ID1cbiAgICAgICAgICAgICAgICAgY2FzZSBvcHRzLndvcmtpbmdEaXJlY3Rvcnkgb2ZcbiAgICAgICAgICAgICAgICAgICAgIEluaGVyaXRXb3JraW5nRGlyZWN0b3J5IC0+IFxuICAgICAgICAgICAgICAgICAgICAgICAgeyBpbmhlcml0ID0gVHJ1ZVxuICAgICAgICAgICAgICAgICAgICAgICAgLCBvdmVycmlkZSA9IFwiXCJcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgICAgICAgICAgU2V0V29ya2luZ0RpcmVjdG9yeSB2YWx1ZSAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgeyBpbmhlcml0ID0gRmFsc2VcbiAgICAgICAgICAgICAgICAgICAgICAgICwgb3ZlcnJpZGUgPSB2YWx1ZVxuICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgLCBlbnZpcm9ubWVudFZhcmlhYmxlcyA9XG4gICAgICAgICAgICAgICAgY2FzZSBvcHRzLmVudmlyb25tZW50VmFyaWFibGVzIG9mXG4gICAgICAgICAgICAgICAgICAgIEluaGVyaXRFbnZpcm9ubWVudFZhcmlhYmxlcyAtPiBcbiAgICAgICAgICAgICAgICAgICAgICAgIHsgb3B0aW9uID0gMFxuICAgICAgICAgICAgICAgICAgICAgICAgLCB2YWx1ZSA9IERpY3QuZW1wdHlcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICBcbiAgICAgICAgICAgICAgICAgICAgTWVyZ2VXaXRoRW52aXJvbm1lbnRWYXJpYWJsZXMgdmFsdWUgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgIHsgb3B0aW9uID0gMVxuICAgICAgICAgICAgICAgICAgICAgICAgLCB2YWx1ZSA9IHZhbHVlXG4gICAgICAgICAgICAgICAgICAgICAgICB9XG5cbiAgICAgICAgICAgICAgICAgICAgUmVwbGFjZUVudmlyb25tZW50VmFyaWFibGVzIHZhbHVlIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICB7IG9wdGlvbiA9IDJcbiAgICAgICAgICAgICAgICAgICAgICAgICwgdmFsdWUgPSB2YWx1ZVxuICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgLCBydW5EdXJhdGlvbiA9XG4gICAgICAgICAgICAgICAgY2FzZSBvcHRzLnJ1bkR1cmF0aW9uIG9mXG4gICAgICAgICAgICAgICAgICAgIE5vTGltaXQgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgIDBcblxuICAgICAgICAgICAgICAgICAgICBNaWxsaXNlY29uZHMgbXMgLT4gXG4gICAgICAgICAgICAgICAgICAgICAgICBtYXggMCBtc1xuICAgICAgICAgICAgLCBjb25uZWN0aW9uID1cbiAgICAgICAgICAgICAgICBjYXNlIG9wdHMuY29ubmVjdGlvbiBvZlxuICAgICAgICAgICAgICAgICAgICBJbnRlZ3JhdGVkIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAwXG5cbiAgICAgICAgICAgICAgICAgICAgSWdub3JlZCAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgMVxuXG4gICAgICAgICAgICAgICAgICAgIERldGFjaGVkIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAyXG4gICAgICAgICAgICB9XG5cblxuey18IFNhbWUgYXMgW3NwYXduXSwgYnV0IHdpdGggW2RlZmF1bHRTcGF3bk9wdGlvbnNdKCNkZWZhdWx0U3Bhd25PcHRpb25zKSBwYXNzZWQgaW4gYXMgb3B0aW9ucy5cbi19XG5zcGF3bldpdGhEZWZhdWx0T3B0aW9ucyA6IFBlcm1pc3Npb24gLT4gU3RyaW5nIC0+IEFycmF5IFN0cmluZyAtPiBUYXNrIHggUHJvY2Vzcy5JZFxuc3Bhd25XaXRoRGVmYXVsdE9wdGlvbnMgcGVybWlzc2lvbiBwcm9ncmFtIGFyZ3VtZW50cyA9XG4gICAgc3Bhd24gcGVybWlzc2lvbiBwcm9ncmFtIGFyZ3VtZW50cyBkZWZhdWx0U3Bhd25PcHRpb25zXG4iLAogICAgICAgICJlZmZlY3QgbW9kdWxlIEh0dHBDbGllbnQgd2hlcmUgeyBjb21tYW5kID0gTXlDbWQgfSBleHBvc2luZ1xuICAgICggUGVybWlzc2lvblxuICAgICwgaW5pdGlhbGl6ZSwgaW5pdGlhbGl6ZUZvckhvc3RcbiAgICAsIFJlcXVlc3RDb25maWd1cmF0aW9uLCBnZXQsIHBvc3QsIHJlcXVlc3RcbiAgICAsIGRlZmF1bHRUaW1lb3V0LCB3aXRoVGltZW91dFxuICAgICwgd2l0aEhlYWRlciwgd2l0aER1cGxpY2F0ZWRIZWFkZXJcbiAgICAsIEJvZHksIHdpdGhFbXB0eUJvZHksIHdpdGhTdHJpbmdCb2R5LCB3aXRoSnNvbkJvZHksIHdpdGhCeXRlc0JvZHlcbiAgICAsIEV4cGVjdCwgZXhwZWN0QW55dGhpbmcsIGV4cGVjdE5vdGhpbmcsIGV4cGVjdFN0cmluZywgZXhwZWN0SnNvbiwgZXhwZWN0Qnl0ZXNcbiAgICAsIHNlbmRcbiAgICAsIFJlc3BvbnNlXG4gICAgLCBFcnJvciguLiksIGVycm9yVG9TdHJpbmdcbiAgICAsIFN0cmVhbVJlcXVlc3QsIFN0cmVhbUV2ZW50KC4uKSwgc3RyZWFtLCBzZW5kQ2h1bmssIHN0YXJ0UmVjZWl2ZSwgYWJvcnRcbiAgICApXG5cblxuey18XG5cbkEgbW9kdWxlIGZvciBjb21tdW5pY2F0aW5nIG92ZXIgSFRUUC5cblxuWW91IHN0YXJ0IGJ5IGJ1aWxkaW5nIGEgW1JlcXVlc3RDb25maWd1cmF0aW9uXSgjUmVxdWVzdENvbmZpZ3VyYXRpb24pIHR5cGUsIHdoaWNoIHJlcHJlc2VudHMgdGhlIHJlcXVlc3QgeW91J2xsIG1ha2UgdG8gYSBzZXJ2ZXIuIE9uY2UgZG9uZSxcbnlvdSBjYW4gZWl0aGVyIGRvIGEgYHNlbmRgLCB3aGljaCByZXByZXNlbnRzIHRoZSByZXNwb25zZSBhcyBhIGBUYXNrYCwgb3IgYHN0cmVhbWAgd2hpY2ggd2lsbCBhbGxvdyB5b3UgdG8gcGVyZm9ybVxuYWN0aW9ucyB3aGlsZSB0aGUgcmVxdWVzdCBpcyBzZW5kaW5nIGFuZCB3aGlsZSB0aGUgcmVzcG9uc2UgaXMgY29taW5nIGluLiBBIHR5cGljYWwgZXhhbXBsZSBvZiB3aHkgeW91J2QgdXNlIGBzdHJlYW1gXG5pcyB0byBzaG93IGEgcHJvZ3Jlc3MgYmFyIHRvIHRoZSB1c2VyLCBvciBkZWNvZGUgdGhlIHJlc3BvbnNlIGluY3JlbWVudGFsbHkgYXMgb3Bwb3NlZCB0byBhbGwgYXQgb25jZS5cblxuU2VlIFtleGFtcGxlcy9odHRwLWNsaWVudF0oaHR0cHM6Ly9naXRodWIuY29tL2dyZW4tbGFuZy9ub2RlL3RyZWUvbWFpbi9leGFtcGxlcy9odHRwLWNsaWVudCkgZm9yIGEgd29ya2luZyBleGFtcGxlLlxuXG4jIyBJbml0aWFsaXphdGlvblxuXG5Db2RlIHRoYXQgd2lzaGVzIHRvIHBlcmZvcm0gSFRUUCByZXF1ZXN0cyByZXF1aXJlIGEgcGVybWlzc2lvbiB0byBkbyBzby5cblxuQGRvY3MgUGVybWlzc2lvbiwgaW5pdGlhbGl6ZSwgaW5pdGlhbGl6ZUZvckhvc3RcblxuIyMgUmVxdWVzdCBjb25maWd1cmF0aW9uXG5cbkluIG9yZGVyIHRvIHNlbmQgc29tZXRoaW5nIG92ZXIgSFRUUCwgeW91IGZpcnN0IG5lZWQgYSBkZXNjcmlwdGlvbiBvZiBob3cgdGhhdCByZXF1ZXN0IHdpbGwgbG9vayBsaWtlLlxuXG5AZG9jcyBSZXF1ZXN0Q29uZmlndXJhdGlvbiwgZ2V0LCBwb3N0LCByZXF1ZXN0XG5cbiMjIFRpbWVvdXRzXG5cbkEgdGltZW91dCByZXByZXNlbnRzIGhvdyBsb25nIHlvdSdyZSB3aWxsaW5nIHRvIHdhaXQgYmVmb3JlIGdpdmluZyB1cCBvbiByZWNlaXZpbmdcbmEgcmVzcG9uc2UgZnJvbSB0aGUgc2VydmVyLiBTZXJ2ZXJzIG1pZ2h0IG5vdCByZXNwb25kIGZvciBhbnkgbnVtYmVyIG9mIHJlYXNvbnMsIGxpa2UgYnVncyBvciBodWdlIGFtb3VudHMgb2YgdHJhZmZpYyxcbnNvIGl0IGlzIGEgZ29vZCBpZGVhIHRvIHJldHVybiBhbiBlcnJvciB0byB0aGUgdXNlciBpbnN0ZWFkIG9mIHdhaXRpbmcgXCJmb3JldmVyXCIgZm9yIGEgcmVzcG9uc2UuXG5cbkBkb2NzIGRlZmF1bHRUaW1lb3V0LCB3aXRoVGltZW91dFxuXG4jIyBIZWFkZXJzXG5cbkV2ZXJ5IEhUVFAgcmVxdWVzdCBjYW4gaGF2ZSBhcmJpdHJhcnkgbWV0YWRhdGEgYXR0YWNoZWQsIGNhbGxlZCBoZWFkZXJzLiBIZWFkZXJzIGFsbG93IHlvdSB0byBhdHRhY2ggdGhpbmdzIGxpa2VcbmF1dGhvcml6YXRpb24gaW5mb3JtYXRpb24sIGhvdyB0aGUgYm9keSBpcyBlbmNvZGVkIG9yIHRoZSBuYW1lIG9mIHRoZSBjbGllbnQgbWFraW5nIHRoZSByZXF1ZXN0LlxuXG5JdCBtaWdodCBiZSBpbnRlcmVzdGluZyB0byByZWFkIHRoaXMgW2xpc3Qgb2YgSFRUUCBoZWFkZXIgZmllbGRzXShodHRwczovL2VuLndpa2lwZWRpYS5vcmcvd2lraS9MaXN0X29mX0hUVFBfaGVhZGVyX2ZpZWxkcykuXG5cbkBkb2NzIHdpdGhIZWFkZXIsIHdpdGhEdXBsaWNhdGVkSGVhZGVyXG5cbiMjIFJlcXVlc3QgYm9keVxuXG5UaGUgcmVxdWVzdCBib2R5IGlzIHRoZSBhY3R1YWwgZGF0YSB0aGF0IHlvdSB3aXNoIHRvIHNlbmQgdG8gYSBzZXJ2ZXIuXG5cbkBkb2NzIEJvZHksIHdpdGhFbXB0eUJvZHksIHdpdGhTdHJpbmdCb2R5LCB3aXRoSnNvbkJvZHksIHdpdGhCeXRlc0JvZHlcblxuIyMgRXhwZWN0ZWQgcmVzcG9uc2UgYm9keVxuXG5PbmNlIGEgcmVxdWVzdCBoYXMgYmVlbiBzZW50LCB5b3UgdXN1YWxseSBnZXQgYSByZXNwb25zZS4gVGhlIGBFeHBlY3RgIHR5cGUgcmVwcmVzZW50c1xud2hhdCB3ZSBleHBlY3QgdGhlIHJlc3BvbnNlIGJvZHkgdG8gYmUuXG5cbkBkb2NzIEV4cGVjdCwgZXhwZWN0QW55dGhpbmcsIGV4cGVjdE5vdGhpbmcsIGV4cGVjdFN0cmluZywgZXhwZWN0SnNvbiwgZXhwZWN0Qnl0ZXNcblxuIyMgU2VuZFxuXG5PbmNlIHlvdXIgYFJlc3BvbnNlYCBpcyBjb25maWd1cmVkLCB5b3UnbGwgd2FudCB0byBhY3R1YWxseSBzZW5kIHRoZSByZXF1ZXN0LlxuXG5AZG9jcyBzZW5kXG5cbkBkb2NzIFJlc3BvbnNlXG5cbiMjIEVycm9yc1xuXG5AZG9jcyBFcnJvciwgZXJyb3JUb1N0cmluZ1xuXG4jIyBTdHJlYW1pbmdcblxuU3RyZWFtaW5nIGlzIHRoZSBtb3JlIGFkdmFuY2VkIHdheSB0byBwZXJmb3JtIGEgSFRUUCByZXF1ZXN0LiBUaGlzIHJlcXVpcmVzIHRoYXQgeW91IGZvbGxvdyB0aGUgRWxtXG5hcmNoaXRlY3R1cmUsIGFzIHlvdSdsbCByZWNlaXZlIG1lc3NhZ2VzIGZvciBldmVyeSBjaHVuayBvZiBkYXRhIHNlbnQgYW5kIHJlY2VpdmVkLiBUaGUgYmVuZWZpdCBvZiB0aGlzXG5leHRyYSBjb21wbGV4aXR5LCBpcyB0aGF0IHlvdSBjYW4gcGVyZm9ybSBhY3Rpb25zIHdoaWxlIHRoZSByZXF1ZXN0IGlzIGJlaW5nIHBlcmZvcm1lZC5cblxuQGRvY3MgU3RyZWFtUmVxdWVzdCwgU3RyZWFtRXZlbnQsIHN0cmVhbSwgc2VuZENodW5rLCBzdGFydFJlY2VpdmUsIGFib3J0XG5cbi19XG5cblxuaW1wb3J0IERpY3QgZXhwb3NpbmcgKERpY3QpXG5pbXBvcnQgSW5pdFxuaW1wb3J0IEludGVybmFsLkluaXRcbmltcG9ydCBKc29uLkVuY29kZSBhcyBKc29uXG5pbXBvcnQgSnNvbi5EZWNvZGVcbmltcG9ydCBCeXRlcyBleHBvc2luZyAoQnl0ZXMpXG5pbXBvcnQgVGFzayBleHBvc2luZyAoVGFzaylcbmltcG9ydCBQbGF0Zm9ybVxuaW1wb3J0IEdyZW4uS2VybmVsLkh0dHBDbGllbnRcbmltcG9ydCBIdHRwU2VydmVyIGV4cG9zaW5nIChNZXRob2QoLi4pLCBtZXRob2RUb1N0cmluZylcblxuXG57LXwgQSB2YWx1ZSB0aGF0IHJlcHJlc2VudHMgdGhlIHBlcm1pc3Npb24gdG8gcGVyZm9ybSBIVFRQIHJlcXVlc3RzLlxuXG5Pbmx5IGNvZGUgeW91IHRydXN0IHNob3VsZCBiZSBncmFudGVkIHBlcm1pc3Npb24uXG4tfVxudHlwZSBQZXJtaXNzaW9uXG4gICAgPSBBbnlQZXJtaXNzaW9uXG4gICAgfCBTcGVjaWZpY1Blcm1pc3Npb24gU3RyaW5nXG5cblxuey18IENhbGwgdGhpcyBkdXJpbmcgYXBwbGljYXRpb24gaW5pdGlhbGl6YXRpb24gdG8gZ2V0IHRoZSBwZXJtaXNzaW9uIHRvIHBlcmZvcm0gYW55IGtpbmQgb2YgSFRUUCByZXF1ZXN0LlxuLX1cbmluaXRpYWxpemUgOiBJbml0LlRhc2sgUGVybWlzc2lvblxuaW5pdGlhbGl6ZSA9XG4gICAgVGFzay5zdWNjZWVkIEFueVBlcm1pc3Npb25cbiAgICAgICAgfD4gSW50ZXJuYWwuSW5pdC5UYXNrXG5cblxuey18IENhbGwgZHVyaW5nIGFwcGxpY2F0aW9uIGluaXRpYWxpemF0aW9uIHRvIGdldCBhIGhvc3Qtc3BlY2lmaWMgcGVybWlzc2lvbi4gQ29kZSB0aGF0IGhhcyB0aGlzIHBlcm1pc3Npb24gdmFsdWUsIHdpbGwgb25seVxuYmUgYWJsZSB0byBjb250YWN0IGEgc3BlY2lmaWMgaG9zdC5cbi19XG5pbml0aWFsaXplRm9ySG9zdCA6IFN0cmluZyAtPiBJbml0LlRhc2sgUGVybWlzc2lvblxuaW5pdGlhbGl6ZUZvckhvc3QgaG9zdCA9XG4gICAgVGFzay5zdWNjZWVkIChTcGVjaWZpY1Blcm1pc3Npb24gaG9zdClcbiAgICAgICAgfD4gSW50ZXJuYWwuSW5pdC5UYXNrXG5cblxuLS0gUkVRVUVTVCBDT05GSUdVUkFUSU9OXG5cblxuey18IERlc2NyaWJlcyB0aGUgcmVxdWVzdCB0byBiZSBtYWRlLiBVc2UgW2dldF0oI2dldCksIFtwb3N0XSgjcG9zdCkgb3IgW3JlcXVlc3RdKCNyZXF1ZXN0KSB0byBpbml0aWFsaXplXG50aGlzIHZhbHVlLCB0aGVuIGN1c3RvbWl6ZSBpdCB1c2luZyB0aGUgZm9sbG93aW5nIGB3aXRoYCBmdW5jdGlvbnMuXG4tfVxudHlwZSBhbGlhcyBSZXF1ZXN0Q29uZmlndXJhdGlvbiByZXNwb25zZVR5cGUgPVxuICAgIHsgbWV0aG9kIDogTWV0aG9kXG4gICAgLCB1cmwgOiBTdHJpbmdcbiAgICAsIGhlYWRlcnMgOiBEaWN0IFN0cmluZyAoQXJyYXkgU3RyaW5nKVxuICAgICwgYm9keSA6IEJvZHlcbiAgICAsIGV4cGVjdCA6IEV4cGVjdCByZXNwb25zZVR5cGVcbiAgICAsIHRpbWVvdXQgOiBJbnRcbiAgICB9XG5cblxuey18IEluaXRpYWxpemVzIHRoZSBjb25maWd1cmF0aW9uIGZvciBhIHNpbXBsZSBHRVQgcmVxdWVzdCB0byB0aGUgZ2l2ZW4gdXJsLlxuLX1cbmdldCA6IFN0cmluZyAtPiBSZXF1ZXN0Q29uZmlndXJhdGlvbiB7fVxuZ2V0IHVybCA9XG4gICAgcmVxdWVzdCBHRVQgdXJsXG5cblxuey18IEluaXRpYWxpemVzIHRoZSBjb25maWd1cmF0aW9uIGZvciBhIHNpbXBsZSBQT1NUIHJlcXVlc3QgdG8gdGhlIGdpdmVuIHVybC5cbi19XG5wb3N0IDogU3RyaW5nIC0+IFJlcXVlc3RDb25maWd1cmF0aW9uIHt9XG5wb3N0IHVybCA9XG4gICAgcmVxdWVzdCBQT1NUIHVybFxuXG5cbnstfCBJbml0aWFsaXplcyBhIHJlcXVlc3QgY29uZmlndXJhdGlvbiB3aXRoIHRoZSBnaXZlbiBtZXRob2QgYW5kIHVybC5cbi19XG5yZXF1ZXN0IDogTWV0aG9kIC0+IFN0cmluZyAtPiBSZXF1ZXN0Q29uZmlndXJhdGlvbiB7fVxucmVxdWVzdCBtZXRob2QgdXJsID1cbiAgICB7IG1ldGhvZCA9IG1ldGhvZFxuICAgICwgdXJsID0gdXJsXG4gICAgLCBoZWFkZXJzID0gRGljdC5lbXB0eVxuICAgICwgYm9keSA9IEJvZHlFbXB0eVxuICAgICwgZXhwZWN0ID0gRXhwZWN0QW55dGhpbmdcbiAgICAsIHRpbWVvdXQgPSBkZWZhdWx0VGltZW91dFxuICAgIH1cblxuXG57LXwgVGhpcyBpcyB0aGUgZGVmYXVsdCB0aW1lb3V0IHZhbHVlLiBJdCBpcyBzZXQgdG8gMTAgc2Vjb25kcy5cbklmIHlvdSBkb24ndCB1c2UgW3dpdGhUaW1lb3V0XSgjd2l0aFRpbWVvdXQpIHRvIHNldCBhIHRpbWVvdXQgc3BlY2lmaWNhbGx5LCBcbnRoaXMgdmFsdWUgd2lsbCBiZSB1c2VkLlxuLX1cbmRlZmF1bHRUaW1lb3V0IDogSW50XG5kZWZhdWx0VGltZW91dCA9XG4gICAgLS0gMTAgc2Vjb25kc1xuICAgIDEwICogMTAwMFxuXG5cbnstfCBMZXRzIHlvdSBzcGVjaWZ5IGEgdGltZW91dCwgaW4gbWlsbGlzZWNvbmRzLCBmb3IgYSByZXF1ZXN0LlxuSWYgdGhlIHNlcnZlciBkb2Vzbid0IHJlc3BvbmQgdG8geW91ciByZXF1ZXN0IHdpdGhpbiB0aGUgZ2l2ZW4gdGltZW91dCwgdGhlIHJlcXVlc3RcbndpbGwgZmFpbCB3aXRoIGEgVGltZW91dCBbRXJyb3JdKCNFcnJvcikuXG4tfVxud2l0aFRpbWVvdXQgOiBJbnQgLT4gUmVxdWVzdENvbmZpZ3VyYXRpb24gYSAtPiBSZXF1ZXN0Q29uZmlndXJhdGlvbiBhXG53aXRoVGltZW91dCBtcyByZXEgPVxuICAgIGlmIG1zIDwgMCB0aGVuXG4gICAgICAgIHJlcVxuXG4gICAgZWxzZVxuICAgICAgICB7IHJlcSB8IHRpbWVvdXQgPSBtcyB9XG5cblxuey18IEEgaGVhZGVyIGlzIGEga2V5LXZhbHVlIHBhaXIgb2Ygc3RyaW5ncyB0aGF0IHNheXMgc29tZXRoaW5nIGFib3V0IHRoZSByZXF1ZXN0LlxuRXhhbXBsZXMgaW5jbHVkZSB0aGUgbGVuZ3RoIG9mIHRoZSBib2R5LCBhdXRoZW50aWNhdGlvbiBpbmZvcm1hdGlvbiwgbmFtZSBvZiB0aGUgY2xpZW50IG1ha2luZyB0aGUgcmVxdWVzdCwgZXRjLlxuLX1cbndpdGhIZWFkZXIgOiBTdHJpbmcgLT4gU3RyaW5nIC0+IFJlcXVlc3RDb25maWd1cmF0aW9uIGEgLT4gUmVxdWVzdENvbmZpZ3VyYXRpb24gYVxud2l0aEhlYWRlciBrZXkgdmFsdWUgcmVxID1cbiAgICB7IHJlcVxuICAgICAgICB8IGhlYWRlcnMgPSBEaWN0LnNldCAoU3RyaW5nLnRvTG93ZXIga2V5KSBbdmFsdWVdIHJlcS5oZWFkZXJzXG4gICAgfVxuXG5cbnstfCBIZWFkZXIga2V5cyBkb2Vzbid0IGhhdmUgdG8gYmUgdW5pcXVlLiBZb3UncmUgYWxsb3dlZCB0byBzZW5kIHRoZSBzYW1lIGtpbmQgb2YgaGVhZGVyXG5tdWx0aXBsZSB0aW1lcywgbGlrZSBzZW5kaW5nIG11bHRpcGxlIGNvb2tpZXMuIFRoZSBiZWhhdmlvdXIgb2YgW3dpdGhIZWFkZXJdKCN3aXRoSGVhZGVyKSB3aWxsXG5yZXBsYWNlIHRoZSB2YWx1ZSBvZiBhbiBhbHJlYWR5IHNldCBoZWFkZXIuIFRoaXMgZnVuY3Rpb24gd2lsbCBub3QuXG4tfVxud2l0aER1cGxpY2F0ZWRIZWFkZXIgOiBTdHJpbmcgLT4gU3RyaW5nIC0+IFJlcXVlc3RDb25maWd1cmF0aW9uIGEgLT4gUmVxdWVzdENvbmZpZ3VyYXRpb24gYVxud2l0aER1cGxpY2F0ZWRIZWFkZXIga2V5IHZhbHVlIHJlcSA9XG4gICAgeyByZXFcbiAgICAgICAgfCBoZWFkZXJzID0gRGljdC51cGRhdGUgXG4gICAgICAgICAgICAoU3RyaW5nLnRvTG93ZXIga2V5KVxuICAgICAgICAgICAgKE1heWJlLm1hcCAoQXJyYXkucHVzaExhc3QgdmFsdWUpID4+IE1heWJlLndpdGhEZWZhdWx0IFt2YWx1ZV0gPj4gSnVzdClcbiAgICAgICAgICAgIHJlcS5oZWFkZXJzXG4gICAgfVxuXG5cbi0tIEJPRFlcblxuXG57LXwgVGhlIGJvZHkgcmVwcmVzZW50cyB0aGUgbWFpbiBkYXRhIHRoYXQgeW91IHdpbGwgc2VuZCBpbiB0aGUgSFRUUCByZXF1ZXN0LlxuLX1cbnR5cGUgQm9keVxuICAgID0gQm9keUVtcHR5XG4gICAgfCBCb2R5U3RyaW5nIFN0cmluZ1xuICAgIHwgQm9keUJ5dGVzIEJ5dGVzXG5cblxuYm9keVR5cGVBc1N0cmluZyA6IEJvZHkgLT4gU3RyaW5nXG5ib2R5VHlwZUFzU3RyaW5nIGJvZHkgPVxuICAgIGNhc2UgYm9keSBvZlxuICAgICAgICBCb2R5RW1wdHkgLT5cbiAgICAgICAgICAgIFwiRU1QVFlcIlxuXG4gICAgICAgIEJvZHlTdHJpbmcgXyAtPlxuICAgICAgICAgICAgXCJTVFJJTkdcIlxuXG4gICAgICAgIEJvZHlCeXRlcyBfIC0+XG4gICAgICAgICAgICBcIkJZVEVTXCJcblxuXG57LXwgUmVtb3ZlcyB0aGUgYm9keSBmcm9tIHRoZSBbUmVxdWVzdENvbmZpZ3VyYXRpb25dKCNSZXF1ZXN0Q29uZmlndXJhdGlvbikuXG5Zb3Ugbm9ybWFsbHkgZG9uJ3QgaGF2ZSB0byB1c2UgdGhpcyBmdW5jdGlvbiwgYXMgYW4gZW1wdHkgYm9keSBpcyB0aGUgZGVmYXVsdC5cblxuSWYgdGhlIFwiQ29udGVudC1UeXBlXCIgaGVhZGVyIGlzIHNldCwgdGhpcyBmdW5jdGlvbiB3aWxsIHJlbW92ZSBpdC5cbi19XG53aXRoRW1wdHlCb2R5IDogUmVxdWVzdENvbmZpZ3VyYXRpb24gYSAtPiBSZXF1ZXN0Q29uZmlndXJhdGlvbiBhXG53aXRoRW1wdHlCb2R5IHJlcSA9IFxuICAgIHsgcmVxXG4gICAgICAgIHwgaGVhZGVycyA9IERpY3QucmVtb3ZlIFwiY29udGVudC10eXBlXCIgcmVxLmhlYWRlcnNcbiAgICAgICAgLCBib2R5ID0gQm9keUVtcHR5XG4gICAgfVxuXG5cbnstfCBTZXRzIHRoZSBnaXZlbiBzdHJpbmcgYXMgdGhlIHJlcXVlc3QgYm9keS4gWW91IG5lZWQgdG8gcHJvdmlkZSBhIG1pbWUgdHlwZSB0b1xuZGVzY3JpYmUgd2hhdCB0aGUgc3RyaW5nIGNvbnRhaW5zLiBUaGlzIG1pbWUgdHlwZSB3aWxsIGJlIHNldCBhcyB0aGUgXCJDb250ZW50LVR5cGVcIlxuaGVhZGVyLCBwb3RlbnRpYWxseSBvdmVyd3JpdGluZyB0aGUgaGVhZGVyIGlmIGl0IGhhcyBhbHJlYWR5IGJlZW4gc2V0LlxuLX1cbndpdGhTdHJpbmdCb2R5IDogU3RyaW5nIC0+IFN0cmluZyAtPiBSZXF1ZXN0Q29uZmlndXJhdGlvbiBhIC0+IFJlcXVlc3RDb25maWd1cmF0aW9uIGFcbndpdGhTdHJpbmdCb2R5IG1pbWVUeXBlIHZhbHVlIHJlcSA9IFxuICAgIHsgcmVxXG4gICAgICAgIHwgaGVhZGVycyA9IERpY3Quc2V0IFwiY29udGVudC10eXBlXCIgW21pbWVUeXBlXSByZXEuaGVhZGVyc1xuICAgICAgICAsIGJvZHkgPSBCb2R5U3RyaW5nIHZhbHVlXG4gICAgfVxuXG5cbnstfCBTZXRzIHRoZSBwcm92aWRlZCBKc29uIHZhbHVlIHRoZSByZXF1ZXN0IGJvZHkuIEEgXCJDb250ZW50LVR5cGVcIiBoZWFkZXIgd2lsbCBiZVxuYXR0YWNoZWQgdG8gdGhlIHJlcXVlc3Qgd2l0aCBhIHZhbHVlIG9mIFwiYXBwbGljYXRpb24vanNvblwiLCBwb3RlbnRpYWxseSBvdmVyd3JpdGluZ1xudGhlIGhlYWRlciBpZiBpdCBoYXMgYWxyZWFkeSBiZWVuIHNldC5cbi19XG53aXRoSnNvbkJvZHkgOiBKc29uLlZhbHVlIC0+IFJlcXVlc3RDb25maWd1cmF0aW9uIGEgLT4gUmVxdWVzdENvbmZpZ3VyYXRpb24gYVxud2l0aEpzb25Cb2R5IHZhbHVlIHJlcSA9IFxuICAgIHdpdGhTdHJpbmdCb2R5IFwiYXBwbGljYXRpb24vanNvblwiIChKc29uLmVuY29kZSAwIHZhbHVlKSByZXFcblxuXG57LXwgU2V0cyB0aGUgcHJvdmlkZWQgQnl0ZXMgdmFsdWUgYXMgdGhlIHJlcXVlc3QgYm9keS4gWW91IG5lZWQgdG8gcHJvdmlkZSBhIG1pbWUgdHlwZSB0b1xuZGVzcmliZSB3aGF0IHRoZSBieXRlcyByZXByZXNlbnQuIFRoaXMgbWltZSB0eXBlIHdpbGwgYmUgc2V0IGFzIHRoZSBcIkNvbnRlbnQtVHlwZVwiIGhlYWRlcixcbnBvdGVudGlhbGx5IG92ZXJ3cml0aW5nIHRoZSBoZWFkZXIgaWYgaXQgaGFzIGFscmVhZHkgYmVlbiBzZXQuXG4tfVxud2l0aEJ5dGVzQm9keSA6IFN0cmluZyAtPiBCeXRlcyAtPiBSZXF1ZXN0Q29uZmlndXJhdGlvbiBhIC0+IFJlcXVlc3RDb25maWd1cmF0aW9uIGFcbndpdGhCeXRlc0JvZHkgbWltZVR5cGUgdmFsdWUgcmVxID0gXG4gICAgeyByZXFcbiAgICAgICAgfCBoZWFkZXJzID0gRGljdC5zZXQgXCJjb250ZW50LXR5cGVcIiBbbWltZVR5cGVdIHJlcS5oZWFkZXJzXG4gICAgICAgICwgYm9keSA9IEJvZHlCeXRlcyB2YWx1ZVxuICAgIH1cblxuXG4tLSBFWFBFQ1RcblxuXG57LXwgVGhpcyBkZXNjcmliZXMgd2hhdCB5b3UgZXhwZWN0IHRoZSBzZXJ2ZXIgd2lsbCByZXNwb25kIHdpdGggd2hlbiBpdCByZWNlaXZlcyB5b3VyIHJlcXVlc3QuXG4tfVxudHlwZSBFeHBlY3QgYVxuICAgID0gRXhwZWN0Tm90aGluZ1xuICAgIHwgRXhwZWN0QW55dGhpbmdcbiAgICB8IEV4cGVjdFN0cmluZ1xuICAgIHwgRXhwZWN0SnNvbiAoSnNvbi5EZWNvZGUuRGVjb2RlciBhKVxuICAgIHwgRXhwZWN0Qnl0ZXNcblxuXG5leHBlY3RUeXBlQXNTdHJpbmcgOiBFeHBlY3QgYSAtPiBTdHJpbmdcbmV4cGVjdFR5cGVBc1N0cmluZyBleHBlY3QgPVxuICAgIGNhc2UgZXhwZWN0IG9mXG4gICAgICAgIEV4cGVjdE5vdGhpbmcgLT5cbiAgICAgICAgICAgIFwiTk9USElOR1wiXG5cbiAgICAgICAgRXhwZWN0QW55dGhpbmcgLT5cbiAgICAgICAgICAgIFwiQU5ZVEhJTkdcIlxuXG4gICAgICAgIEV4cGVjdFN0cmluZyAtPlxuICAgICAgICAgICAgXCJTVFJJTkdcIlxuXG4gICAgICAgIEV4cGVjdEpzb24gXyAtPlxuICAgICAgICAgICAgXCJKU09OXCJcblxuICAgICAgICBFeHBlY3RCeXRlcyAtPlxuICAgICAgICAgICAgXCJCWVRFU1wiXG5cblxuey18IFVzZSB0aGlzIHdoZW4geW91IHlvdSBkb24ndCByZWFsbHkgY2FyZSB3aGF0IHRoZSBzZXJ2ZXIgcmVzcG9uZHMgd2l0aC4gQW55dGhpbmcgaXMgZmluZS5cbkFjdHVhbGx5LCB0aGlzIGlzIHRoZSBkZWZhdWx0IHZhbHVlIHNvIHlvdSBwcm9iYWJseSBkb24ndCBuZWVkIHRvIHVzZSB0aGlzIGF0IGFsbC5cbi19XG5leHBlY3RBbnl0aGluZyA6IFJlcXVlc3RDb25maWd1cmF0aW9uIGEgLT4gUmVxdWVzdENvbmZpZ3VyYXRpb24ge31cbmV4cGVjdEFueXRoaW5nIHJlcSA9XG4gICAgLS0gTmVlZCB0byBjcmVhdGUgYSBuZXcgcmVjb3JkIGZvciB0eXBlIGNoZWNraW5nIHRvIHBhc3NcbiAgICB7IG1ldGhvZCA9IHJlcS5tZXRob2RcbiAgICAsIHVybCA9IHJlcS51cmxcbiAgICAsIGhlYWRlcnMgPSByZXEuaGVhZGVyc1xuICAgICwgYm9keSA9IHJlcS5ib2R5XG4gICAgLCBleHBlY3QgPSBFeHBlY3RBbnl0aGluZ1xuICAgICwgdGltZW91dCA9IHJlcS50aW1lb3V0XG4gICAgfVxuXG5cbnstfCBFeHBlY3QgX2V4YWN0bHlfIG5vdGhpbmcuIFVzZSB0aGlzIHdoZW4geW91IHdhbnQgYSByZXF1ZXN0IHRvIGZhaWwgaWYgdGhlIHNlcnZlciByZXNwb25kcyB3aXRoXG5hbnl0aGluZyBhdCBhbGwuXG4tfVxuZXhwZWN0Tm90aGluZyA6IFJlcXVlc3RDb25maWd1cmF0aW9uIGEgLT4gUmVxdWVzdENvbmZpZ3VyYXRpb24ge31cbmV4cGVjdE5vdGhpbmcgcmVxID1cbiAgICB7IG1ldGhvZCA9IHJlcS5tZXRob2RcbiAgICAsIHVybCA9IHJlcS51cmxcbiAgICAsIGhlYWRlcnMgPSByZXEuaGVhZGVyc1xuICAgICwgYm9keSA9IHJlcS5ib2R5XG4gICAgLCBleHBlY3QgPSBFeHBlY3ROb3RoaW5nXG4gICAgLCB0aW1lb3V0ID0gcmVxLnRpbWVvdXRcbiAgICB9XG5cblxuey18IFVzZSB0aGlzIHdoZW4geW91IGV4cGVjdCB0aGUgc2VydmVyIHRvIHJlc3BvbmQgd2l0aCBhIHN0cmluZy5cbi19XG5leHBlY3RTdHJpbmcgOiBSZXF1ZXN0Q29uZmlndXJhdGlvbiBhIC0+IFJlcXVlc3RDb25maWd1cmF0aW9uIFN0cmluZ1xuZXhwZWN0U3RyaW5nIHJlcSA9XG4gICAgeyBtZXRob2QgPSByZXEubWV0aG9kXG4gICAgLCB1cmwgPSByZXEudXJsXG4gICAgLCBoZWFkZXJzID0gcmVxLmhlYWRlcnNcbiAgICAsIGJvZHkgPSByZXEuYm9keVxuICAgICwgZXhwZWN0ID0gRXhwZWN0U3RyaW5nXG4gICAgLCB0aW1lb3V0ID0gcmVxLnRpbWVvdXRcbiAgICB9XG5cblxuey18IFVzZSB0aGlzIHdoZW4geW91IGV4cGVjdCBhIEpzb24gcmVzcG9uc2UuIFRoZSByZXF1ZXN0IHdpbGwgZmFpbCBpZiB0aGUgcHJvdmlkZWQgZGVjb2RlciBmYWlscy5cbi19XG5leHBlY3RKc29uIDogSnNvbi5EZWNvZGUuRGVjb2RlciBhIC0+IFJlcXVlc3RDb25maWd1cmF0aW9uIHggLT4gUmVxdWVzdENvbmZpZ3VyYXRpb24gYVxuZXhwZWN0SnNvbiBkZWNvZGVyIHJlcSA9IFxuICAgIHsgbWV0aG9kID0gcmVxLm1ldGhvZFxuICAgICwgdXJsID0gcmVxLnVybFxuICAgICwgaGVhZGVycyA9IHJlcS5oZWFkZXJzXG4gICAgLCBib2R5ID0gcmVxLmJvZHlcbiAgICAsIGV4cGVjdCA9IEV4cGVjdEpzb24gZGVjb2RlclxuICAgICwgdGltZW91dCA9IHJlcS50aW1lb3V0XG4gICAgfVxuXG5cbnstfCBVc2UgdGhpcyB3aGVuIHlvdSB3YW50IHRvIHRyZWF0IHRoZSByZXNwb25zZSBhcyBieXRlcy4gVGhpcyB3aWxsIGxpa2VseSBuZXZlciBmYWlsLCBhcyBhbnl0aGluZ1xuY2FuIGJlIGludGVycHJldGVkIGFzIGJ5dGVzLlxuLX1cbmV4cGVjdEJ5dGVzIDogUmVxdWVzdENvbmZpZ3VyYXRpb24gYSAtPiBSZXF1ZXN0Q29uZmlndXJhdGlvbiBCeXRlc1xuZXhwZWN0Qnl0ZXMgcmVxID1cbiAgICB7IG1ldGhvZCA9IHJlcS5tZXRob2RcbiAgICAsIHVybCA9IHJlcS51cmxcbiAgICAsIGhlYWRlcnMgPSByZXEuaGVhZGVyc1xuICAgICwgYm9keSA9IHJlcS5ib2R5XG4gICAgLCBleHBlY3QgPSBFeHBlY3RCeXRlc1xuICAgICwgdGltZW91dCA9IHJlcS50aW1lb3V0XG4gICAgfVxuXG5cbi0tIFNJTVBMRSBTRU5EXG5cblxuey18IFNlbmQgYSByZXF1ZXN0LiBUaGUgdGFzayB3aWxsIGVpdGhlciBjb21wbGV0ZSB3aXRoIGEgc3VjY2Vzc2Z1bCBbUmVzcG9uc2VdKCNSZXNwb25zZSksIG9yIGFuIFtFcnJvcl0oI0Vycm9yKS5cbi19XG5zZW5kXG4gICAgOiBQZXJtaXNzaW9uIFxuICAgIC0+IFJlcXVlc3RDb25maWd1cmF0aW9uIGV4cGVjdGVkQm9keVxuICAgIC0+IFRhc2sgKEVycm9yIGV4cGVjdGVkQm9keSkgKFJlc3BvbnNlIGV4cGVjdGVkQm9keSlcbnNlbmQgcGVybWlzc2lvbiBjb25maWcgPVxuICAgIEdyZW4uS2VybmVsLkh0dHBDbGllbnQucmVxdWVzdCAoa2VybmVsUmVxdWVzdENvbmZpZyBwZXJtaXNzaW9uIGNvbmZpZylcblxuXG50eXBlIGFsaWFzIEtlcm5lbFJlcXVlc3RDb25maWcgYSA9XG4gICAgeyBtZXRob2QgOiBTdHJpbmdcbiAgICAsIHVybCA6IFN0cmluZ1xuICAgICwgaGVhZGVycyA6IERpY3QgU3RyaW5nIChBcnJheSBTdHJpbmcpXG4gICAgLCBib2R5VHlwZSA6IFN0cmluZ1xuICAgICwgYm9keSA6IEJvZHlcbiAgICAsIGV4cGVjdFR5cGUgOiBTdHJpbmdcbiAgICAsIGV4cGVjdCA6IEV4cGVjdCBhXG4gICAgLCB0aW1lb3V0IDogSW50XG4gICAgfVxuXG5cbmtlcm5lbFJlcXVlc3RDb25maWcgOiBQZXJtaXNzaW9uIC0+IFJlcXVlc3RDb25maWd1cmF0aW9uIGEgLT4gS2VybmVsUmVxdWVzdENvbmZpZyBhXG5rZXJuZWxSZXF1ZXN0Q29uZmlnIHBlcm1pc3Npb24gY29uZmlnID1cbiAgICBsZXRcbiAgICAgICAgYWN0dWFsVXJsID1cbiAgICAgICAgICAgIGNhc2UgcGVybWlzc2lvbiBvZlxuICAgICAgICAgICAgICAgIEFueVBlcm1pc3Npb24gLT5cbiAgICAgICAgICAgICAgICAgICAgY29uZmlnLnVybFxuXG4gICAgICAgICAgICAgICAgU3BlY2lmaWNQZXJtaXNzaW9uIHByZWZpeCAtPlxuICAgICAgICAgICAgICAgICAgICBwcmVmaXggKysgY29uZmlnLnVybFxuICAgIGluXG4gICAgeyBtZXRob2QgPSBtZXRob2RUb1N0cmluZyBjb25maWcubWV0aG9kXG4gICAgLCB1cmwgPSBhY3R1YWxVcmxcbiAgICAsIGhlYWRlcnMgPSBjb25maWcuaGVhZGVyc1xuICAgICwgYm9keVR5cGUgPSBib2R5VHlwZUFzU3RyaW5nIGNvbmZpZy5ib2R5XG4gICAgLCBib2R5ID0gY29uZmlnLmJvZHlcbiAgICAsIGV4cGVjdFR5cGUgPSBleHBlY3RUeXBlQXNTdHJpbmcgY29uZmlnLmV4cGVjdFxuICAgICwgZXhwZWN0ID0gY29uZmlnLmV4cGVjdFxuICAgICwgdGltZW91dCA9IGNvbmZpZy50aW1lb3V0XG4gICAgfVxuXG5cbi0tIFJFU1BPTlNFXG5cblxuey18IFRoZSByZXNwb25zZSBmcm9tIHRoZSBzZXJ2ZXIuXG5cbiogc3RhdHVzQ29kZTogQSBudW1lcmljYWwgdmFsdWUgdGhhdCBnaXZlcyBhbiBpbmRpY2F0aW9uIG9mIGhvdyB0aGUgcmVxdWVzdCB3ZW50LlxuSXQgbWlnaHQgYmUgYSBnb29kIGlkZWEgdG8gcmVhZCB0aGlzIFtsaXN0IG9mIEhUVFAgc3RhdHVzIGNvZGVzXShodHRwczovL2VuLndpa2lwZWRpYS5vcmcvd2lraS9MaXN0X29mX0hUVFBfc3RhdHVzX2NvZGVzKS5cbiogc3RhdHVzVGV4dDogQSBodW1hbiByZWFkYWJsZSBpbnRlcnByZXRhdGlvbiBvZiB0aGUgc3RhdHVzIGNvZGUuXG4qIGhlYWRlcnM6IFRoZSBoZWFkZXJzIHJldHVybmVkIGJ5IHRoZSBzZXJ2ZXIuXG4qIGRhdGE6IFRoZSBkYXRhIHJldHVybmVkIGJ5IHRoZSBzZXJ2ZXIuIFRoZSB0eXBlIGRlcGVuZHMgb24gdGhlIFtFeHBlY3RdKCNFeHBlY3QpIHZhbHVlIHlvdSBzZXQgb24gdGhlIHJlcXVlc3QuXG4tfVxudHlwZSBhbGlhcyBSZXNwb25zZSBkYXRhID1cbiAgICB7IHN0YXR1c0NvZGUgOiBJbnRcbiAgICAsIHN0YXR1c1RleHQgOiBTdHJpbmdcbiAgICAsIGhlYWRlcnMgOiBEaWN0IFN0cmluZyAoQXJyYXkgU3RyaW5nKVxuICAgICwgZGF0YSA6IGRhdGFcbiAgICB9XG5cblxuLS0gRVJST1JTXG5cblxuey18IEEgSFRUUCByZXF1ZXN0IGNhbiBmYWlsIGluIGEgbnVtYmVyIG9mIHdheXMuXG5cbiogQmFkVXJsOiBTb21ldGhpbmcgaXMgd3Jvbmcgd2l0aCB0aGUgVVJMIHlvdSBwcm92aWRlZC5cbiogQmFkSGVhZGVyczogVGhlIHJlcXVlc3QgaGVhZGVycyBhcmUgaW52YWxpZC4gTWFrZSBzdXJlIHlvdSBvbmx5IHVzZSBjaGFyYWN0ZXJzIGluIHRoZSBsYXRpbi0xIGNoYXJhY3RlciBzZXQuXG4qIEJhZFN0YXR1czogVGhlIHN0YXR1cyBjb2RlIGluZGljYXRlcyB0aGF0IHRoZSByZXNwb25zZSBkaWRuJ3QgZ28gd2VsbC4gVGhlIFtSZXNwb25zZV0oI1Jlc3BvbnNlKSBpcyBhdHRhY2hlZCwgd2l0aCBhIHN0cmluZy1lbmNvZGVkIGRhdGEuXG4qIFRpbWVvdXQ6IFRoZSByZXF1ZXN0IHRpbWVkIG91dC4gVGhlIHNlcnZlciBkaWRuJ3QgcmVzcG9uZCBhcyBxdWlja2x5IGFzIHlvdSBleHBlY3RlZCBpdCB3b3VsZC5cbiogVW5rbm93bkVycm9yOiBXZSBkb24ndCBrbm93IHdoYXQgd2VudCB3cm9uZy4gWW91IHNob3VsZCBwcm9iYWJseSByZXBvcnQgdGhpcyBpZiB5b3Ugc2VlIGl0IGluIHRoZSB3aWxkLlxuLX1cbnR5cGUgRXJyb3IgYm9keVxuICAgID0gQmFkVXJsIFN0cmluZ1xuICAgIHwgQmFkSGVhZGVyc1xuICAgIHwgQmFkU3RhdHVzIChSZXNwb25zZSBib2R5KVxuICAgIHwgVW5leHBlY3RlZFJlc3BvbnNlQm9keSBTdHJpbmdcbiAgICB8IFRpbWVvdXRcbiAgICB8IFVua25vd25FcnJvciBTdHJpbmdcblxuXG57LXwgR2l2ZXMgYSBicmllZiBkZXNjcmlwdGlvbiBvZiBhbiBlcnJvci5cbi19XG5lcnJvclRvU3RyaW5nIDogRXJyb3IgYm9keSAtPiBTdHJpbmdcbmVycm9yVG9TdHJpbmcgZXJyID1cbiAgICBjYXNlIGVyciBvZlxuICAgICAgICBUaW1lb3V0IC0+XG4gICAgICAgICAgICBcIlRpbWVvdXRcIlxuXG4gICAgICAgIEJhZFVybCB1cmwgLT5cbiAgICAgICAgICAgIFwiQmFkIFVSTDogXCIgKysgdXJsXG5cbiAgICAgICAgQmFkSGVhZGVycyAtPlxuICAgICAgICAgICAgXCJCYWQgaGVhZGVyczogb25lIG9yIG1vcmUgb2YgeW91ciBoZWFkZXJzIGNvbnRhaW5zIGludmFsaWQgY2hhcmFjdGVycy5cIlxuXG4gICAgICAgIEJhZFN0YXR1cyByZXMgLT5cbiAgICAgICAgICAgIFwiQmFkIHN0YXR1czogXCIgKysgU3RyaW5nLmZyb21JbnQgcmVzLnN0YXR1c0NvZGUgKysgXCIgLSBcIiArKyByZXMuc3RhdHVzVGV4dFxuXG4gICAgICAgIFVuZXhwZWN0ZWRSZXNwb25zZUJvZHkgbWVzc2FnZSAtPlxuICAgICAgICAgICAgXCJVbmV4cGVjdGVkIHJlc3BvbnNlIGJvZHk6IFwiICsrIG1lc3NhZ2VcblxuICAgICAgICBVbmtub3duRXJyb3IgZGVidWdTdHIgLT5cbiAgICAgICAgICAgIFwiVW5rbm93biBlcnJvcjogXCIgKysgZGVidWdTdHJcblxuXG4tLSBTVFJFQU1cblxuXG57LXwgSWRlbnRpZmllcyBhIHN0cmVhbWluZyByZXF1ZXN0LiBSZXF1aXJlZCB0byBwZXJmb3JtIGNlcnRhaW4gb3BlcmF0aW9ucyB3aGlsZVxudGhlIHJlcXVlc3QgaXMgc3RyZWFtaW5nLlxuLX1cbnR5cGUgU3RyZWFtUmVxdWVzdCA9XG4gICAgU3RyZWFtUmVxdWVzdCBJbnRcblxuXG57LXwgV2hlbiBhIHJlcXVlc3QgaXMgc3RyZWFtaW5nLCB0aGUgYXBwbGljYXRpb24gaXMgbm90aWZpZWQgb2YgaW1wb3J0YW50IGV2ZW50c1xuYW5kIGlzIHJlcXVpcmVkIHRvIGFjdCBvbiB0aG9zZSBldmVudHMgZm9yIHRoZSByZXF1ZXN0IHRvIGJlIHN1Y2Nlc3NmdWwuXG5cbiogU2VudENodW5rOiBUaGUgaW5pdGlhbCByZXF1ZXN0IGJvZHksIG9yIHRoZSBsYXN0IHBpZWNlIG9mIGRhdGEgc2VudCB3aXRoIFtzZW5kQ2h1bmtdKCNzZW5kQ2h1bmspIGhhcyBiZWVuIHNlbnQuXG5TZW5kIG1vcmUgZGF0YSwgb3IgY2FsbCBgc3RhcnRSZWNlaXZlYCB0byBiZWdpbiBsaXN0ZW5pbmcgZm9yIHRoZSByZXNwb25zZS5cbiogUmVjZWl2ZWRDaHVuazogVGhlIHNlcnZlciBoYXMgcmVzcG9uZGVkIHdpdGggc29tZSBkYXRhLiBNb3JlIGRhdGEgbWlnaHQgYmUgY29taW5nIGluLCB0aG91Z2guXG5UaGUgYERvbmVgIGV2ZW50IHdpbGwgYmUgdHJpZ2dlcmVkIHdoZW4gdGhlcmUncyBubyBtb3JlIGRhdGEgY29taW5nLiBZb3UgY2FuIHVzZSB0aGUgcHJvdmlkZWRcbmBSZXNwb25zZWAgb2JqZWN0IHRvIGFjY2VzcyB0aGUgcmVzcG9uc2UgaGVhZGVycywgYW5kIGRlY2lkZSBpZiB5b3UnZCBsaWtlIHRvIFthYm9ydF0oI2Fib3J0KSB0aGVcbnJlcXVlc3Qgb3Igbm90LlxuKiBFcnJvcjogU29tZXRoaW5nIHdlbnQgd3JvbmcuIE1vcmUgaW5mb3JtYXRpb24gaW4gdGhlIHByb3ZpZGVkIFtFcnJvcl0oI0Vycm9yKSBvYmplY3QuXG4qIEFib3J0ZWQ6IFlvdSBjYWxsZWQgW2Fib3J0XSgjYWJvcnQpIG9uIHRoaXMgcmVxdWVzdC5cbiogRG9uZTogVGhlIHNlcnZlciBoYXMgc2VudCBhbGwgaXQncyBnb2luZyB0byBzZW5kLiBZb3UncmUgZG9uZS5cbi19XG50eXBlIFN0cmVhbUV2ZW50XG4gICAgPSBTZW50Q2h1bmsgU3RyZWFtUmVxdWVzdFxuICAgIHwgUmVjZWl2ZWRDaHVuayBTdHJlYW1SZXF1ZXN0IChSZXNwb25zZSBCeXRlcylcbiAgICB8IEVycm9yIChFcnJvciBCeXRlcylcbiAgICB8IEFib3J0ZWRcbiAgICB8IERvbmVcblxuXG57LXwgSW5pdGlhbGl6ZSBhIHN0cmVhbWluZyByZXF1ZXN0LiBZb3UgbmVlZCB0byBwcm92aWRlIGEgZnVuY3Rpb24gdGhhdCBnZW5lcmF0ZXMgYSBtZXNzYWdlXG5mb3IgaGFuZGxpbmcgW1N0cmVhbUV2ZW50XSgjU3RyZWFtRXZlbnQpcy4gVGhlIGhlYWRlcnMgYW5kIGRhdGEgd2lsbCBiZSBzZW50IHRvIHRoZSBzZXJ2ZXJcbmltbWVkaWV0bHksIGFuZCBhIGBTZW50Q2h1bmtgIGV2ZW50IHdpbGwgYmUgc2VudCB0aGV5J3JlIGRvbmUuXG5cblRvIHRlbGwgZGlmZmVyZW50IHJlcXVlc3RzIGFwYXJ0LCB5b3UgY2FuIHVzZSBhIHBhcnRpYWxseSBhcHBsaWVkIGN1c3RvbSB0eXBlIGxpa2UgdGhpczpcblxuICAgIHR5cGUgTXNnID0gSHR0cFJlcXVlc3QgU3RyaW5nIFN0cmVhbUV2ZW50XG5cbiAgICBIdHRwQ2xpZW50LnN0cmVhbSBodHRwUGVybWlzc2lvbiAoSHR0cFJlcXVlc3QgXCJSZXF1ZXN0IDFcIikgcmVxdWVzdENvbmZpZ1xuLX1cbnN0cmVhbSA6IFBlcm1pc3Npb24gLT4gKFN0cmVhbUV2ZW50IC0+IG1zZykgLT4gUmVxdWVzdENvbmZpZ3VyYXRpb24gQnl0ZXMgLT4gQ21kIG1zZ1xuc3RyZWFtIHBlcm1pc3Npb24gdG9Nc2cgY29uZmlnID1cbiAgICBjb21tYW5kIDx8IFN0YXJ0IHRvTXNnIDx8IGtlcm5lbFJlcXVlc3RDb25maWcgcGVybWlzc2lvbiBjb25maWdcblxuXG57LXwgU2VuZCBtb3JlIGRhdGEgdG8gdGhlIHNlcnZlci4gVGhpcyBhbGxvd3MgeW91IHRvIGdlbmVyYXRlIG1vcmUgZGF0YSBhcyB5b3UgbmVlZCB0bywgZW5hYmxpbmdcbnlvdSBzbGljZSB1cCBhIHBvdGVudGlhbGx5IGNvc3RseSwgbWVtb3J5IGhlYXZ5IG9yIGxvbmctcnVubmluZyBvcGVyYXRpb24gb3ZlciB0aW1lLlxuXG5Zb3UgZG9uJ3QgaGF2ZSB0byB3YWl0IGZvciB0aGUgbWF0Y2hpbmcgYFNlbnRDaHVua2AgZXZlbnQgYmVmb3JlIHNlbmRpbmcgbW9yZSBkYXRhIGJ1dCBrZWVwIGluXG5taW5kIHRoYXQgZGF0YSB3aWxsIGJlIGtlcHQgaW4gbWVtb3J5IHVudGlsIHNlbnQsIHBvdGVudGlhbGx5IGNhdXNpbmcgb3V0LW9mLW1lbW9yeSBlcnJvcnMgaW5cbnRoZSBjYXNlIG9mIGxhcmdlIGFtb3VudHMgb2YgZGF0YS5cblxuSWYgeW91J3JlIGFscmVhZHkgcmVjZWl2aW5nIGRhdGEgZnJvbSB0aGUgc2VydmVyLCBjYWxsaW5nIHRoaXMgZnVuY3Rpb24gd2lsbCBubyBlZmZlY3QuXG4tfVxuc2VuZENodW5rIDogU3RyZWFtUmVxdWVzdCAtPiBCeXRlcyAtPiBDbWQgbXNnXG5zZW5kQ2h1bmsgcmVxIGJ5dGVzID1cbiAgICBjb21tYW5kIDx8IFNlbmRDaHVuayBieXRlcyByZXFcblxuXG57LXwgVXNlIHRoaXMgd2hlbiB5b3UncmUgZG9uZSBzZW5kaW5nIGRhdGEuIFRoZSBzZXJ2ZXIgd2lsbCBub3cgYmVnaW4gc3RyZWFtaW5nIHlvdSB0aGUgcmVzcG9uc2UuXG4tfVxuc3RhcnRSZWNlaXZlIDogU3RyZWFtUmVxdWVzdCAtPiBDbWQgbXNnXG5zdGFydFJlY2VpdmUgcmVxID1cbiAgICBjb21tYW5kIDx8IFN0YXJ0UmVjZWl2ZSByZXFcblxuXG57LXwgU3RvcHMgdGhlIHJlcXVlc3QsIGZvciBhbnkgcmVhc29uLCBhdCBhbnkgdGltZS4gVXNlZnVsIGlmIHlvdSBoYXZlIGFuIHVuZXhwZWN0ZWQgZXJyb3Igd2l0aFxueW91ciBvd24gc291cmNlIG9mIGRhdGEsIG9yIGlmIHRoZSBzZXJ2ZXIgcmVzcG9uc2UgaXMgb25lIHlvdSBrbm93IHlvdSBkb24ndCB3YW50IHRvIGhhbmRsZSBhZnRlclxuaGF2aW5nIGluc3BlY3RlZCB0aGUgaGVhZGVycy5cbi19XG5hYm9ydCA6IFN0cmVhbVJlcXVlc3QgLT4gQ21kIGFcbmFib3J0IHJlcSA9XG4gICAgY29tbWFuZCA8fCBBYm9ydCByZXFcblxuXG4tLSBDT01NQU5EU1xuXG5cbnR5cGUgTXlDbWQgbXNnXG4gICAgPSBTdGFydCAoU3RyZWFtRXZlbnQgLT4gbXNnKSAoS2VybmVsUmVxdWVzdENvbmZpZyBCeXRlcylcbiAgICB8IFNlbmRDaHVuayBCeXRlcyBTdHJlYW1SZXF1ZXN0XG4gICAgfCBTdGFydFJlY2VpdmUgU3RyZWFtUmVxdWVzdFxuICAgIHwgQWJvcnQgU3RyZWFtUmVxdWVzdFxuXG5cbmNtZE1hcCA6IChhIC0+IGIpIC0+IE15Q21kIGEgLT4gTXlDbWQgYlxuY21kTWFwIGZ1bmMgY21kID1cbiAgICBjYXNlIGNtZCBvZlxuICAgICAgICBTdGFydCB0b01zZyByZXEgLT5cbiAgICAgICAgICAgIFN0YXJ0ICh0b01zZyA+PiBmdW5jKSByZXFcblxuICAgICAgICBTZW5kQ2h1bmsgYnl0ZXMgcmVxIC0+XG4gICAgICAgICAgICBTZW5kQ2h1bmsgYnl0ZXMgcmVxXG4gICAgICAgIFxuICAgICAgICBTdGFydFJlY2VpdmUgcmVxIC0+XG4gICAgICAgICAgICBTdGFydFJlY2VpdmUgcmVxXG5cbiAgICAgICAgQWJvcnQgcmVxIC0+XG4gICAgICAgICAgICBBYm9ydCByZXFcblxuXG50eXBlIGFsaWFzIFJlcXVlc3RzU3RhdGUgbXNnID1cbiAgICB7IG5leHRJZCA6IEludCBcbiAgICAsIG1zZ0hhbmRsZXJzIDogRGljdCBJbnQgeyB0b01zZyA6IChTdHJlYW1FdmVudCAtPiBtc2cpLCBrZXJuZWxSZXF1ZXN0IDogS2VybmVsUmVxdWVzdCB9XG4gICAgfVxuXG5cbnR5cGUgS2VybmVsUmVxdWVzdCA9IFxuICAgIC0tIEFjdHVhbCBpbXBsZW1lbnRhdGlvbiBwcm92aWRlZCBieSBrZXJuZWwgY29kZVxuICAgIEtlcm5lbFJlcXVlc3RcblxuXG5pbml0IDogVGFzayBOZXZlciAoUmVxdWVzdHNTdGF0ZSBtc2cpXG5pbml0ID1cbiAgVGFzay5zdWNjZWVkXG4gICAgeyBuZXh0SWQgPSAwXG4gICAgLCBtc2dIYW5kbGVycyA9IERpY3QuZW1wdHlcbiAgICB9XG5cblxub25FZmZlY3RzIDogUGxhdGZvcm0uUm91dGVyIG1zZyBTZWxmTXNnIC0+IEFycmF5IChNeUNtZCBtc2cpIC0+IFJlcXVlc3RzU3RhdGUgbXNnIC0+IFRhc2sgTmV2ZXIgKFJlcXVlc3RzU3RhdGUgbXNnKVxub25FZmZlY3RzIHJvdXRlciBjb21tYW5kcyBzdGF0ZSA9XG4gIGNhc2UgQXJyYXkucG9wRmlyc3QgY29tbWFuZHMgb2ZcbiAgICBOb3RoaW5nIC0+XG4gICAgICBUYXNrLnN1Y2NlZWQgc3RhdGVcblxuICAgIEp1c3QgeyBmaXJzdCwgcmVzdCB9IC0+XG4gICAgICAgIGNhc2UgZmlyc3Qgb2ZcbiAgICAgICAgICAgIFN0YXJ0IHRvTXNnIGNvbmZpZyAtPlxuICAgICAgICAgICAgICAgIGxldFxuICAgICAgICAgICAgICAgICAgICBzdHJlYW1pbmdSZXEgPVxuICAgICAgICAgICAgICAgICAgICAgICAgU3RyZWFtUmVxdWVzdCBzdGF0ZS5uZXh0SWRcbiAgICAgICAgICAgICAgICBpblxuICAgICAgICAgICAgICAgIEdyZW4uS2VybmVsLkh0dHBDbGllbnQuc3RyZWFtIFxuICAgICAgICAgICAgICAgICAgICAoUGxhdGZvcm0uc2VuZFRvU2VsZiByb3V0ZXIgPDwgQ2xlYW51cCkgXG4gICAgICAgICAgICAgICAgICAgIChQbGF0Zm9ybS5zZW5kVG9BcHAgcm91dGVyIDw8IHRvTXNnKSBcbiAgICAgICAgICAgICAgICAgICAgc3RyZWFtaW5nUmVxIFxuICAgICAgICAgICAgICAgICAgICBjb25maWdcbiAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5tYXAgXG4gICAgICAgICAgICAgICAgICAgICAgICAoXFxrZXJuZWxSZXF1ZXN0IC0+IFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHsgc3RhdGUgXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHwgbmV4dElkID0gc3RhdGUubmV4dElkICsgMVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAsIG1zZ0hhbmRsZXJzID0gXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBEaWN0LnNldCBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBzdGF0ZS5uZXh0SWRcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB7IHRvTXNnID0gdG9Nc2dcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAsIGtlcm5lbFJlcXVlc3QgPSBrZXJuZWxSZXF1ZXN0XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHN0YXRlLm1zZ0hhbmRsZXJzXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgKVxuICAgICAgICAgICAgICAgICAgICB8PiBUYXNrLmFuZFRoZW4gKFxcdXBkYXRlZFN0YXRlIC0+IG9uRWZmZWN0cyByb3V0ZXIgcmVzdCB1cGRhdGVkU3RhdGUpXG5cbiAgICAgICAgICAgIFNlbmRDaHVuayBieXRlcyAoKFN0cmVhbVJlcXVlc3QgcmVxSWQpIGFzIHJlcSkgLT5cbiAgICAgICAgICAgICAgICBjYXNlIERpY3QuZ2V0IHJlcUlkIHN0YXRlLm1zZ0hhbmRsZXJzIG9mXG4gICAgICAgICAgICAgICAgICAgIEp1c3QgbXNnSGFuZGxlciAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgR3Jlbi5LZXJuZWwuSHR0cENsaWVudC5zZW5kQ2h1bmsgXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgKFBsYXRmb3JtLnNlbmRUb0FwcCByb3V0ZXIgPDwgbXNnSGFuZGxlci50b01zZykgXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgbXNnSGFuZGxlci5rZXJuZWxSZXF1ZXN0XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgcmVxIFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGJ5dGVzXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5hbmRUaGVuIChcXF8gLT4gb25FZmZlY3RzIHJvdXRlciByZXN0IHN0YXRlKVxuICAgICAgICAgICAgICAgICAgICBcbiAgICAgICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgb25FZmZlY3RzIHJvdXRlciByZXN0IHN0YXRlXG4gICAgICAgICAgICBcbiAgICAgICAgICAgIFN0YXJ0UmVjZWl2ZSAoKFN0cmVhbVJlcXVlc3QgcmVxSWQpIGFzIHJlcSktPlxuICAgICAgICAgICAgICAgIGNhc2UgRGljdC5nZXQgcmVxSWQgc3RhdGUubXNnSGFuZGxlcnMgb2ZcbiAgICAgICAgICAgICAgICAgICAgSnVzdCBtc2dIYW5kbGVyIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICBHcmVuLktlcm5lbC5IdHRwQ2xpZW50LnN0YXJ0UmVjZWl2ZVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIChQbGF0Zm9ybS5zZW5kVG9TZWxmIHJvdXRlciA8PCBDbGVhbnVwKVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIChQbGF0Zm9ybS5zZW5kVG9BcHAgcm91dGVyIDw8IG1zZ0hhbmRsZXIudG9Nc2cpIFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIG1zZ0hhbmRsZXIua2VybmVsUmVxdWVzdFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHJlcSBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB8PiBUYXNrLmFuZFRoZW4gKFxcXyAtPiBvbkVmZmVjdHMgcm91dGVyIHJlc3Qgc3RhdGUpXG4gICAgICAgICAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICBvbkVmZmVjdHMgcm91dGVyIHJlc3Qgc3RhdGVcblxuICAgICAgICAgICAgQWJvcnQgKFN0cmVhbVJlcXVlc3QgcmVxSWQpLT5cbiAgICAgICAgICAgICAgICBjYXNlIERpY3QuZ2V0IHJlcUlkIHN0YXRlLm1zZ0hhbmRsZXJzIG9mXG4gICAgICAgICAgICAgICAgICAgIEp1c3QgbXNnSGFuZGxlciAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgR3Jlbi5LZXJuZWwuSHR0cENsaWVudC5hYm9ydCBtc2dIYW5kbGVyLmtlcm5lbFJlcXVlc3RcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB8PiBUYXNrLmFuZFRoZW4gKFxcXyAtPiBvbkVmZmVjdHMgcm91dGVyIHJlc3Qgc3RhdGUpXG4gICAgICAgICAgICAgICAgICAgIFxuICAgICAgICAgICAgICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICBvbkVmZmVjdHMgcm91dGVyIHJlc3Qgc3RhdGVcbiAgICAgICAgICAgICAgICBcblxudHlwZSBTZWxmTXNnXG4gICAgPSBDbGVhbnVwIFN0cmVhbVJlcXVlc3RcblxuXG5vblNlbGZNc2cgOiBQbGF0Zm9ybS5Sb3V0ZXIgbXNnIFNlbGZNc2cgLT4gU2VsZk1zZyAtPiBSZXF1ZXN0c1N0YXRlIG1zZyAtPiBUYXNrIE5ldmVyIChSZXF1ZXN0c1N0YXRlIG1zZylcbm9uU2VsZk1zZyBfIGV2ZW50IHN0YXRlID1cbiAgICBjYXNlIGV2ZW50IG9mXG4gICAgICAgIENsZWFudXAgKFN0cmVhbVJlcXVlc3QgcmVxSWQpIC0+XG4gICAgICAgICAgICBUYXNrLnN1Y2NlZWQgXG4gICAgICAgICAgICAgICAgeyBzdGF0ZSB8IG1zZ0hhbmRsZXJzID0gRGljdC5yZW1vdmUgcmVxSWQgc3RhdGUubXNnSGFuZGxlcnMgfVxuIiwKICAgICAgICAiZWZmZWN0IG1vZHVsZSBUZXJtaW5hbCB3aGVyZSB7IHN1YnNjcmlwdGlvbiA9IFRlcm1pbmFsU3ViIH0gZXhwb3NpbmdcbiAgICAoIFBlcm1pc3Npb25cbiAgICAsIENvbmZpZ3VyYXRpb25cbiAgICAsIFNpemVcbiAgICAsIGluaXRpYWxpemVcbiAgICAtLVxuICAgICwgc2V0U3RkSW5SYXdNb2RlXG4gICAgLCBzZXRQcm9jZXNzVGl0bGVcbiAgICAtLVxuICAgICwgb25SZXNpemVcbiAgICApXG5cblxuey18IFRoaXMgbGV0cyB5b3UgaW50ZXJhY3Qgd2l0aCB0aGUgdXNlcidzIHRlcm1pbmFsLCBpZiBhbiBpbnRlcmFjdGl2ZVxudGVybWluYWwgaXMgY29ubmVjdGVkIHRvIHRoaXMgYXBwbGljYXRpb24uXG5cbiMjIEluaXRpYWxpemF0aW9uXG5cbkBkb2NzIFBlcm1pc3Npb24sIENvbmZpZ3VyYXRpb24sIFNpemUsIGluaXRpYWxpemVcblxuIyMgQ29tbWFuZHNcblxuQGRvY3Mgc2V0U3RkSW5SYXdNb2RlLCBzZXRQcm9jZXNzVGl0bGVcblxuIyMgU3Vic2NyaXB0aW9uc1xuXG5AZG9jcyBvblJlc2l6ZVxuXG4tfVxuXG5cbmltcG9ydCBUYXNrIGV4cG9zaW5nIChUYXNrKVxuaW1wb3J0IFByb2Nlc3NcbmltcG9ydCBHcmVuLktlcm5lbC5UZXJtaW5hbFxuaW1wb3J0IEluaXRcbmltcG9ydCBJbnRlcm5hbC5Jbml0IFxuXG5cbnstfCBUaGUgcGVybWlzc2lvbiBmb3IgcGVyZm9ybWluZyBjb21tYW5kcyBzcGVjaWZpZWQgaW4gdGhpcyBtb2R1bGUuXG4tfVxudHlwZSBQZXJtaXNzaW9uXG4gICAgPSBQZXJtaXNzaW9uXG5cblxuey18IFRoZSBjb25maWd1cmF0aW9uIG9mIHRoZSBhdHRhY2hlZCBpbnRlcmFjdGl2ZSB0ZXJtaW5hbC5cbi19XG50eXBlIGFsaWFzIENvbmZpZ3VyYXRpb24gPVxuICAgIHsgcGVybWlzc2lvbiA6IFBlcm1pc3Npb25cbiAgICAsIGNvbG9yRGVwdGggOiBJbnRcbiAgICAsIGNvbHVtbnMgOiBJbnRcbiAgICAsIHJvd3MgOiBJbnRcbiAgICB9XG5cblxuey18IFNpemUgb2YgYSB0ZXJtaW5hbC4gSGFuZHkgdG8ga25vdyBmb3IgZHJhd2luZyBhIHRleHQtYmFzZWQgVUkuXG4tfVxudHlwZSBhbGlhcyBTaXplID1cbiAgICB7IGNvbHVtbnMgOiBJbnRcbiAgICAsIHJvd3MgOiBJbnRcbiAgICB9XG5cblxuLS0gSU5JVFxuXG5cbnstfCBJbml0aWFsaXplcyB0aGUgYFRlcm1pbmFsYCBzdWJzeXN0ZW0uXG5cbmBOb3RoaW5nYCBpcyByZXR1cm5lZCBpZiB0aGlzIHByb2dyYW0gaXNuJ3QgY29ubmVjdGVkIHRvIGFuIGludGVyYWN0aXZlIHRlcm1pbmFsLCB3aGljaFxuY2FuIGhhcHBlbiBpbiBDSS1zZXR1cHMgb3Igd2hlbiB1c2VkIGFzIHBhcnQgb2YgYSB1bml4IHBpcGUuXG4tfVxuaW5pdGlhbGl6ZSA6IEluaXQuVGFzayAoTWF5YmUgQ29uZmlndXJhdGlvbilcbmluaXRpYWxpemUgPVxuICAgIEdyZW4uS2VybmVsLlRlcm1pbmFsLmluaXRcbiAgICAgICAgfD4gVGFzay5tYXAgKFxccmF3IC0+XG4gICAgICAgICAgICBpZiByYXcuaXNUVFkgdGhlblxuICAgICAgICAgICAgICAgIEp1c3RcbiAgICAgICAgICAgICAgICAgICAgeyBwZXJtaXNzaW9uID0gUGVybWlzc2lvblxuICAgICAgICAgICAgICAgICAgICAsIGNvbG9yRGVwdGggPSByYXcuY29sb3JEZXB0aFxuICAgICAgICAgICAgICAgICAgICAsIGNvbHVtbnMgPSByYXcuY29sdW1uc1xuICAgICAgICAgICAgICAgICAgICAsIHJvd3MgPSByYXcucm93c1xuICAgICAgICAgICAgICAgICAgICB9XG5cbiAgICAgICAgICAgIGVsc2VcbiAgICAgICAgICAgICAgICBOb3RoaW5nXG4gICAgICAgIClcbiAgICAgICAgfD4gSW50ZXJuYWwuSW5pdC5UYXNrXG5cblxuLS0gQ09NTUFORFNcblxuXG57LXwgSW4gaXQncyBkZWZhdWx0IG1vZGUsIGBzdGRpbmAgb25seSBzZW5kcyBkYXRhIHdoZW4gdGhlIHVzZXIgaGl0cyB0aGUgZW50ZXIga2V5LlxuXG5JZiB5b3Ugc3dpdGNoIG92ZXIgdG8gcmF3IG1vZGUsIGV2ZXJ5IGtleXByZXNzIHdpbGwgYmUgc2VudCBvdmVyIHRoZSBzdHJlYW0sIGFuZCBzcGVjaWFsXG5jb21iaW5hdGlvbnMgbGlrZSBgQ3RybC1DYCB3aWxsIG5vIGxvbmdlciB0cmlnZ2VyIHRoZSBraWxsIHNpZ25hbC5cblxuRW5hYmxlIHRoaXMgd2hlbiB5b3UgbmVlZCBmdWxsIGNvbnRyb2wgb3ZlciBob3cgaW5wdXQgaXMgaGFuZGxlZC5cbi19XG5zZXRTdGRJblJhd01vZGUgOiBQZXJtaXNzaW9uIC0+IEJvb2wgLT4gVGFzayB4IHt9XG5zZXRTdGRJblJhd01vZGUgXyB0b2dnbGUgPVxuICAgIEdyZW4uS2VybmVsLlRlcm1pbmFsLnNldFN0ZEluUmF3TW9kZSB0b2dnbGVcblxuXG57LXwgU2V0IHRoZSB0aXRsZSBvZiB0aGUgcnVubmluZyBwcm9jZXNzLiBUaGlzIHdpbGwgdXN1YWxseSBkaXNwbGF5IGluXG5hY3Rpdml0eSBtb25pdG9ycyBvciBpbiB0aGUgdGl0bGUgYmFyIG9mIHlvdXIgdGVybWluYWwgZW11bGF0b3IuXG4tfVxuc2V0UHJvY2Vzc1RpdGxlIDogUGVybWlzc2lvbiAtPiBTdHJpbmcgLT4gVGFzayB4IHt9XG5zZXRQcm9jZXNzVGl0bGUgXyB0aXRsZSA9XG4gICAgR3Jlbi5LZXJuZWwuVGVybWluYWwuc2V0UHJvY2Vzc1RpdGxlIHRpdGxlXG5cblxuLS0gU1VCU0NSSVBUSU9OU1xuXG5cbnR5cGUgVGVybWluYWxTdWIgbXNnXG4gICAgPSBPblJlc2l6ZSAoU2l6ZSAtPiBtc2cpXG5cblxuc3ViTWFwIDogKGEgLT4gYikgLT4gVGVybWluYWxTdWIgYSAtPiBUZXJtaW5hbFN1YiBiXG5zdWJNYXAgbWFwRm4gc3ViID1cbiAgICBjYXNlIHN1YiBvZlxuICAgICAgICBPblJlc2l6ZSBtc2dNYXAgLT5cbiAgICAgICAgICAgIE9uUmVzaXplIChtYXBGbiA8PCBtc2dNYXApXG5cblxuey18IEEgc3Vic2NyaXB0aW9uIHRoYXQgdHJpZ2dlcnMgZXZlcnkgdGltZSB0aGUgc2l6ZSBvZiB0aGUgdGVybWluYWwgY2hhbmdlcy5cbi19XG5vblJlc2l6ZSA6IFBlcm1pc3Npb24gLT4gKFNpemUgLT4gbXNnKSAtPiBTdWIgbXNnXG5vblJlc2l6ZSBfIHRvTXNnID1cbiAgICBzdWJzY3JpcHRpb24gKE9uUmVzaXplIHRvTXNnKVxuXG5cbi0tIExPT1BcblxuXG50eXBlIGFsaWFzIFN0YXRlIG1zZyA9XG4gICAgeyB0YWdnZXJzIDogQXJyYXkgKFNpemUgLT4gbXNnKVxuICAgICwgbWF5YmVQcm9jZXNzSWQgOiBNYXliZSBQcm9jZXNzLklkXG4gICAgfVxuXG5cbmluaXQgOiBUYXNrIE5ldmVyIChTdGF0ZSBtc2cpXG5pbml0ID1cbiAgICBUYXNrLnN1Y2NlZWRcbiAgICAgICAgeyB0YWdnZXJzID0gW11cbiAgICAgICAgLCBtYXliZVByb2Nlc3NJZCA9IE5vdGhpbmdcbiAgICAgICAgfVxuXG5cbm9uRWZmZWN0c1xuICAgIDogUGxhdGZvcm0uUm91dGVyIG1zZyBFdmVudFxuICAgIC0+IEFycmF5IChUZXJtaW5hbFN1YiBtc2cpXG4gICAgLT4gU3RhdGUgbXNnXG4gICAgLT4gVGFzayBOZXZlciAoU3RhdGUgbXNnKVxub25FZmZlY3RzIHJvdXRlciBzdWJzIHN0YXRlID1cbiAgICBsZXRcbiAgICAgICAgbmV3VGFnZ2VycyA9XG4gICAgICAgICAgICBBcnJheS5mb2xkbCBleHRyYWN0VGFnZ2VyIFtdIHN1YnNcblxuICAgICAgICBleHRyYWN0VGFnZ2VyIChPblJlc2l6ZSB0YWdnZXIpIGFjYyA9XG4gICAgICAgICAgICBBcnJheS5wdXNoTGFzdCB0YWdnZXIgYWNjXG5cbiAgICAgICAgc3Vic2NyaXB0aW9uVGFzayA9XG4gICAgICAgICAgICBpZiBBcnJheS5sZW5ndGggbmV3VGFnZ2VycyA+IDAgdGhlblxuICAgICAgICAgICAgICAgIGNhc2Ugc3RhdGUubWF5YmVQcm9jZXNzSWQgb2ZcbiAgICAgICAgICAgICAgICAgICAgSnVzdCBwaWQgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgIFRhc2suc3VjY2VlZCA8fCBKdXN0IHBpZFxuXG4gICAgICAgICAgICAgICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgIEdyZW4uS2VybmVsLlRlcm1pbmFsLmF0dGFjaExpc3RlbmVyIChcXGRhdGEgLT4gUGxhdGZvcm0uc2VuZFRvU2VsZiByb3V0ZXIgKFNlbGZPblJlc2l6ZSBkYXRhKSlcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICB8PiBQcm9jZXNzLnNwYXduXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5tYXAgSnVzdFxuXG5cbiAgICAgICAgICAgIGVsc2VcbiAgICAgICAgICAgICAgICBjYXNlIHN0YXRlLm1heWJlUHJvY2Vzc0lkIG9mXG4gICAgICAgICAgICAgICAgICAgIEp1c3QgcGlkIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICBQcm9jZXNzLmtpbGwgcGlkXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgfD4gVGFzay5tYXAgKFxcXyAtPiBOb3RoaW5nKVxuXG4gICAgICAgICAgICAgICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgIFRhc2suc3VjY2VlZCBOb3RoaW5nXG4gICAgaW5cbiAgICBzdWJzY3JpcHRpb25UYXNrXG4gICAgICAgIHw+IFRhc2suYW5kVGhlbiAoXFxtYXliZVByb2Nlc3NJZCAtPlxuICAgICAgICAgICAgVGFzay5zdWNjZWVkXG4gICAgICAgICAgICAgICAgeyB0YWdnZXJzID0gbmV3VGFnZ2Vyc1xuICAgICAgICAgICAgICAgICwgbWF5YmVQcm9jZXNzSWQgPSBtYXliZVByb2Nlc3NJZFxuICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgIClcblxuXG50eXBlIEV2ZW50XG4gICAgPSBTZWxmT25SZXNpemUgU2l6ZVxuXG5cbm9uU2VsZk1zZyA6IFBsYXRmb3JtLlJvdXRlciBtc2cgRXZlbnQgLT4gRXZlbnQgLT4gU3RhdGUgbXNnIC0+IFRhc2sgTmV2ZXIgKFN0YXRlIG1zZylcbm9uU2VsZk1zZyByb3V0ZXIgZXZlbnQgc3RhdGUgPVxuICAgIGNhc2UgZXZlbnQgb2ZcbiAgICAgICAgU2VsZk9uUmVzaXplIG5ld1NpemUgLT5cbiAgICAgICAgICAgIHN0YXRlLnRhZ2dlcnNcbiAgICAgICAgICAgICAgICB8PiBBcnJheS5tYXAgKFxcdGFnZ2VyIC0+IHRhZ2dlciBuZXdTaXplKVxuICAgICAgICAgICAgICAgIHw+IEFycmF5LmZvbGRsXG4gICAgICAgICAgICAgICAgICAgIChcXG1zZyB0YXNrcyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgVGFzay5hbmRUaGVuIChcXHt9IC0+IFBsYXRmb3JtLnNlbmRUb0FwcCByb3V0ZXIgbXNnKSB0YXNrc1xuICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICAgICAgICAgIChUYXNrLnN1Y2NlZWQge30pXG4gICAgICAgICAgICAgICAgfD4gVGFzay5tYXAgKFxcXyAtPiBzdGF0ZSlcbiIsCiAgICAgICAgIm1vZHVsZSBNYXliZSBleHBvc2luZ1xuICAgICggTWF5YmUoLi4pXG4gICAgLCB3aXRoRGVmYXVsdCwgbWFwLCBtYXAyLCBtYXAzLCBtYXA0LCBtYXA1XG4gICAgLCBhbmRUaGVuXG4gICAgKVxuXG57LXwgVGhpcyBsaWJyYXJ5IGZpbGxzIGEgYnVuY2ggb2YgaW1wb3J0YW50IG5pY2hlcyBpbiBHcmVuLiBBIGBNYXliZWAgY2FuIGhlbHBcbnlvdSB3aXRoIG9wdGlvbmFsIGFyZ3VtZW50cywgZXJyb3IgaGFuZGxpbmcsIGFuZCByZWNvcmRzIHdpdGggb3B0aW9uYWwgZmllbGRzLlxuXG5cbkBkb2NzIE1heWJlXG5cblxuIyMgQ29tbW9uIEhlbHBlcnNcblxuQGRvY3Mgd2l0aERlZmF1bHQsIG1hcCwgbWFwMiwgbWFwMywgbWFwNCwgbWFwNVxuXG5cbiMjIENoYWluaW5nIE1heWJlc1xuXG5AZG9jcyBhbmRUaGVuXG5cbi19XG5cbmltcG9ydCBCYXNpY3MgZXhwb3NpbmcgKEJvb2woLi4pKVxuXG5cbnstfCBSZXByZXNlbnQgdmFsdWVzIHRoYXQgbWF5IG9yIG1heSBub3QgZXhpc3QuIEl0IGNhbiBiZSB1c2VmdWwgaWYgeW91IGhhdmUgYVxucmVjb3JkIGZpZWxkIHRoYXQgaXMgb25seSBmaWxsZWQgaW4gc29tZXRpbWVzLiBPciBpZiBhIGZ1bmN0aW9uIHRha2VzIGEgdmFsdWVcbnNvbWV0aW1lcywgYnV0IGRvZXMgbm90IGFic29sdXRlbHkgbmVlZCBpdC5cblxuICAgIC0tIEEgcGVyc29uLCBidXQgbWF5YmUgd2UgZG8gbm90IGtub3cgdGhlaXIgYWdlLlxuICAgIHR5cGUgYWxpYXMgUGVyc29uID1cbiAgICAgICAgeyBuYW1lIDogU3RyaW5nXG4gICAgICAgICwgYWdlIDogTWF5YmUgSW50XG4gICAgICAgIH1cblxuICAgIHRvbSA9XG4gICAgICAgIHsgbmFtZSA9IFwiVG9tXCIsIGFnZSA9IEp1c3QgNDIgfVxuXG4gICAgc3VlID1cbiAgICAgICAgeyBuYW1lID0gXCJTdWVcIiwgYWdlID0gTm90aGluZyB9XG5cbi19XG50eXBlIE1heWJlIGFcbiAgICA9IEp1c3QgYVxuICAgIHwgTm90aGluZ1xuXG5cbnstfCBQcm92aWRlIGEgZGVmYXVsdCB2YWx1ZSwgdHVybmluZyBhbiBvcHRpb25hbCB2YWx1ZSBpbnRvIGEgbm9ybWFsXG52YWx1ZS4gVGhpcyBjb21lcyBpbiBoYW5keSB3aGVuIHBhaXJlZCB3aXRoIGZ1bmN0aW9ucyBsaWtlXG5bYERpY3QuZ2V0YF0oRGljdCNnZXQpIHdoaWNoIGdpdmVzIGJhY2sgYSBgTWF5YmVgLlxuXG4gICAgd2l0aERlZmF1bHQgMTAwIChKdXN0IDQyKSAtLSA0MlxuXG4gICAgd2l0aERlZmF1bHQgMTAwIE5vdGhpbmcgLS0gMTAwXG5cbiAgICB3aXRoRGVmYXVsdCBcInVua25vd25cIiAoRGljdC5nZXQgXCJUb21cIiBEaWN0LmVtcHR5KSAtLSBcInVua25vd25cIlxuXG4qKk5vdGU6KiogVGhpcyBjYW4gYmUgb3ZlcnVzZWQhIE1hbnkgY2FzZXMgYXJlIGJldHRlciBoYW5kbGVkIGJ5IGEgYGNhc2VgXG5leHByZXNzaW9uLiBBbmQgaWYgeW91IGVuZCB1cCB1c2luZyBgd2l0aERlZmF1bHRgIGEgbG90LCBpdCBjYW4gYmUgYSBnb29kIHNpZ25cbnRoYXQgYSBbY3VzdG9tIHR5cGVdW2N0XSB3aWxsIGNsZWFuIHlvdXIgY29kZSB1cCBxdWl0ZSBhIGJpdCFcblxuW2N0XTogaHR0cHM6Ly9ndWlkZS5ncmVuLWxhbmcub3JnL3R5cGVzL2N1c3RvbV90eXBlcy5odG1sXG5cbi19XG53aXRoRGVmYXVsdCA6IGEgLT4gTWF5YmUgYSAtPiBhXG53aXRoRGVmYXVsdCBkZWZhdWx0IG1heWJlID1cbiAgICBjYXNlIG1heWJlIG9mXG4gICAgICAgIEp1c3QgdmFsdWUgLT5cbiAgICAgICAgICAgIHZhbHVlXG5cbiAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgZGVmYXVsdFxuXG5cbnstfCBUcmFuc2Zvcm0gYSBgTWF5YmVgIHZhbHVlIHdpdGggYSBnaXZlbiBmdW5jdGlvbjpcblxuICAgIG1hcCBzcXJ0IChKdXN0IDkpID09IEp1c3QgM1xuXG4gICAgbWFwIHNxcnQgTm90aGluZyA9PSBOb3RoaW5nXG5cbiAgICBtYXAgc3FydCAoU3RyaW5nLnRvRmxvYXQgXCI5XCIpID09IEp1c3QgM1xuXG4gICAgbWFwIHNxcnQgKFN0cmluZy50b0Zsb2F0IFwieFwiKSA9PSBOb3RoaW5nXG5cbi19XG5tYXAgOiAoYSAtPiBiKSAtPiBNYXliZSBhIC0+IE1heWJlIGJcbm1hcCBmIG1heWJlID1cbiAgICBjYXNlIG1heWJlIG9mXG4gICAgICAgIEp1c3QgdmFsdWUgLT5cbiAgICAgICAgICAgIEp1c3QgKGYgdmFsdWUpXG5cbiAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgTm90aGluZ1xuXG5cbnstfCBBcHBseSBhIGZ1bmN0aW9uIGlmIGFsbCB0aGUgYXJndW1lbnRzIGFyZSBgSnVzdGAgYSB2YWx1ZS5cblxuICAgIG1hcDIgKCspIChKdXN0IDMpIChKdXN0IDQpID09IEp1c3QgN1xuXG4gICAgbWFwMiAoKykgKEp1c3QgMykgTm90aGluZyA9PSBOb3RoaW5nXG5cbiAgICBtYXAyICgrKSBOb3RoaW5nIChKdXN0IDQpID09IE5vdGhpbmdcblxuICAgIG1hcDIgKCspIChTdHJpbmcudG9JbnQgXCIxXCIpIChTdHJpbmcudG9JbnQgXCIxMjNcIikgPT0gSnVzdCAxMjRcblxuICAgIG1hcDIgKCspIChTdHJpbmcudG9JbnQgXCJ4XCIpIChTdHJpbmcudG9JbnQgXCIxMjNcIikgPT0gTm90aGluZ1xuXG4gICAgbWFwMiAoKykgKFN0cmluZy50b0ludCBcIjFcIikgKFN0cmluZy50b0ludCBcIjEuM1wiKSA9PSBOb3RoaW5nXG5cbi19XG5tYXAyIDogKGEgLT4gYiAtPiB2YWx1ZSkgLT4gTWF5YmUgYSAtPiBNYXliZSBiIC0+IE1heWJlIHZhbHVlXG5tYXAyIGZ1bmMgbWEgbWIgPVxuICAgIGNhc2UgbWEgb2ZcbiAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgTm90aGluZ1xuXG4gICAgICAgIEp1c3QgYSAtPlxuICAgICAgICAgICAgY2FzZSBtYiBvZlxuICAgICAgICAgICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgICAgICAgICAgTm90aGluZ1xuXG4gICAgICAgICAgICAgICAgSnVzdCBiIC0+XG4gICAgICAgICAgICAgICAgICAgIEp1c3QgKGZ1bmMgYSBiKVxuXG5cbnstfCAtfVxubWFwMyA6IChhIC0+IGIgLT4gYyAtPiB2YWx1ZSkgLT4gTWF5YmUgYSAtPiBNYXliZSBiIC0+IE1heWJlIGMgLT4gTWF5YmUgdmFsdWVcbm1hcDMgZnVuYyBtYSBtYiBtYyA9XG4gICAgY2FzZSBtYSBvZlxuICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgSnVzdCBhIC0+XG4gICAgICAgICAgICBjYXNlIG1iIG9mXG4gICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgICAgICAgICBKdXN0IGIgLT5cbiAgICAgICAgICAgICAgICAgICAgY2FzZSBtYyBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIE5vdGhpbmdcblxuICAgICAgICAgICAgICAgICAgICAgICAgSnVzdCBjIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgSnVzdCAoZnVuYyBhIGIgYylcblxuXG57LXwgLX1cbm1hcDQgOiAoYSAtPiBiIC0+IGMgLT4gZCAtPiB2YWx1ZSkgLT4gTWF5YmUgYSAtPiBNYXliZSBiIC0+IE1heWJlIGMgLT4gTWF5YmUgZCAtPiBNYXliZSB2YWx1ZVxubWFwNCBmdW5jIG1hIG1iIG1jIG1kID1cbiAgICBjYXNlIG1hIG9mXG4gICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgIE5vdGhpbmdcblxuICAgICAgICBKdXN0IGEgLT5cbiAgICAgICAgICAgIGNhc2UgbWIgb2ZcbiAgICAgICAgICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICAgICAgICAgIE5vdGhpbmdcblxuICAgICAgICAgICAgICAgIEp1c3QgYiAtPlxuICAgICAgICAgICAgICAgICAgICBjYXNlIG1jIG9mXG4gICAgICAgICAgICAgICAgICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgTm90aGluZ1xuXG4gICAgICAgICAgICAgICAgICAgICAgICBKdXN0IGMgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICBjYXNlIG1kIG9mXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIE5vdGhpbmdcblxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBKdXN0IGQgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIEp1c3QgKGZ1bmMgYSBiIGMgZClcblxuXG57LXwgLX1cbm1hcDUgOiAoYSAtPiBiIC0+IGMgLT4gZCAtPiBlIC0+IHZhbHVlKSAtPiBNYXliZSBhIC0+IE1heWJlIGIgLT4gTWF5YmUgYyAtPiBNYXliZSBkIC0+IE1heWJlIGUgLT4gTWF5YmUgdmFsdWVcbm1hcDUgZnVuYyBtYSBtYiBtYyBtZCBtZSA9XG4gICAgY2FzZSBtYSBvZlxuICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgSnVzdCBhIC0+XG4gICAgICAgICAgICBjYXNlIG1iIG9mXG4gICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgICAgICAgICBKdXN0IGIgLT5cbiAgICAgICAgICAgICAgICAgICAgY2FzZSBtYyBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgIE5vdGhpbmdcblxuICAgICAgICAgICAgICAgICAgICAgICAgSnVzdCBjIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgY2FzZSBtZCBvZlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgSnVzdCBkIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBjYXNlIG1lIG9mXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBOb3RoaW5nXG5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBKdXN0IGUgLT5cbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgSnVzdCAoZnVuYyBhIGIgYyBkIGUpXG5cblxuey18IENoYWluIHRvZ2V0aGVyIG1hbnkgY29tcHV0YXRpb25zIHRoYXQgbWF5IGZhaWwuIEl0IGlzIGhlbHBmdWwgdG8gc2VlIGl0c1xuZGVmaW5pdGlvbjpcblxuICAgIGFuZFRoZW4gOiAoYSAtPiBNYXliZSBiKSAtPiBNYXliZSBhIC0+IE1heWJlIGJcbiAgICBhbmRUaGVuIGNhbGxiYWNrIG1heWJlID1cbiAgICAgICAgY2FzZSBtYXliZSBvZlxuICAgICAgICAgICAgSnVzdCB2YWx1ZSAtPlxuICAgICAgICAgICAgICAgIGNhbGxiYWNrIHZhbHVlXG5cbiAgICAgICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgICAgICBOb3RoaW5nXG5cblRoaXMgbWVhbnMgd2Ugb25seSBjb250aW51ZSB3aXRoIHRoZSBjYWxsYmFjayBpZiB0aGluZ3MgYXJlIGdvaW5nIHdlbGwuIEZvclxuZXhhbXBsZSwgc2F5IHlvdSBuZWVkIHRvIHBhcnNlIHNvbWUgdXNlciBpbnB1dCBhcyBhIG1vbnRoOlxuXG4gICAgcGFyc2VNb250aCA6IFN0cmluZyAtPiBNYXliZSBJbnRcbiAgICBwYXJzZU1vbnRoIHVzZXJJbnB1dCA9XG4gICAgICAgIFN0cmluZy50b0ludCB1c2VySW5wdXRcbiAgICAgICAgICAgIHw+IGFuZFRoZW4gdG9WYWxpZE1vbnRoXG5cbiAgICB0b1ZhbGlkTW9udGggOiBJbnQgLT4gTWF5YmUgSW50XG4gICAgdG9WYWxpZE1vbnRoIG1vbnRoID1cbiAgICAgICAgaWYgMSA8PSBtb250aCAmJiBtb250aCA8PSAxMiB0aGVuXG4gICAgICAgICAgICBKdXN0IG1vbnRoXG5cbiAgICAgICAgZWxzZVxuICAgICAgICAgICAgTm90aGluZ1xuXG5JbiB0aGUgYHBhcnNlTW9udGhgIGZ1bmN0aW9uLCBpZiBgU3RyaW5nLnRvSW50YCBwcm9kdWNlcyBgTm90aGluZ2AgKGJlY2F1c2VcbnRoZSBgdXNlcklucHV0YCB3YXMgbm90IGFuIGludGVnZXIpIHRoaXMgZW50aXJlIGNoYWluIG9mIG9wZXJhdGlvbnMgd2lsbFxuc2hvcnQtY2lyY3VpdCBhbmQgcmVzdWx0IGluIGBOb3RoaW5nYC4gSWYgYHRvVmFsaWRNb250aGAgcmVzdWx0cyBpbiBgTm90aGluZ2AsXG5hZ2FpbiB0aGUgY2hhaW4gb2YgY29tcHV0YXRpb25zIHdpbGwgcmVzdWx0IGluIGBOb3RoaW5nYC5cblxuLX1cbmFuZFRoZW4gOiAoYSAtPiBNYXliZSBiKSAtPiBNYXliZSBhIC0+IE1heWJlIGJcbmFuZFRoZW4gY2FsbGJhY2sgbWF5YmVWYWx1ZSA9XG4gICAgY2FzZSBtYXliZVZhbHVlIG9mXG4gICAgICAgIEp1c3QgdmFsdWUgLT5cbiAgICAgICAgICAgIGNhbGxiYWNrIHZhbHVlXG5cbiAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgTm90aGluZ1xuXG5cblxuLS0gRk9SIElOVEVSTkFMIFVTRSBPTkxZXG4tLVxuLS0gVXNlIGBjYXNlYCBleHByZXNzaW9ucyBmb3IgdGhpcyBpbiBHcmVuIGNvZGUhXG5cblxuaXNKdXN0IDogTWF5YmUgYSAtPiBCb29sXG5pc0p1c3QgbWF5YmUgPVxuICAgIGNhc2UgbWF5YmUgb2ZcbiAgICAgICAgSnVzdCBfIC0+XG4gICAgICAgICAgICBUcnVlXG5cbiAgICAgICAgTm90aGluZyAtPlxuICAgICAgICAgICAgRmFsc2VcblxuXG5kZXN0cnVjdCA6IGIgLT4gKGEgLT4gYikgLT4gTWF5YmUgYSAtPiBiXG5kZXN0cnVjdCBkZWZhdWx0IGZ1bmMgbWF5YmUgPVxuICAgIGNhc2UgbWF5YmUgb2ZcbiAgICAgICAgSnVzdCBhIC0+XG4gICAgICAgICAgICBmdW5jIGFcblxuICAgICAgICBOb3RoaW5nIC0+XG4gICAgICAgICAgICBkZWZhdWx0XG4iLAogICAgICAgICJtb2R1bGUgQnl0ZXMuRW5jb2RlIGV4cG9zaW5nXG4gICggZW5jb2RlXG4gICwgRW5jb2RlclxuICAsIHNpZ25lZEludDgsIHNpZ25lZEludDE2LCBzaWduZWRJbnQzMlxuICAsIHVuc2lnbmVkSW50OCwgdW5zaWduZWRJbnQxNiwgdW5zaWduZWRJbnQzMlxuICAsIGZsb2F0MzIsIGZsb2F0NjRcbiAgLCBieXRlc1xuICAsIHNlcXVlbmNlXG4gIClcblxuXG57LXwgRnVuY3Rpb25zIGZvciB0dXJuaW5nIHRoaW5ncyBpbnRvIGJ5dGVzLlxuXG5AZG9jcyBFbmNvZGVyLCBlbmNvZGUsIHNlcXVlbmNlXG5cbiMjIEludGVnZXJzXG5AZG9jcyBzaWduZWRJbnQ4LCBzaWduZWRJbnQxNiwgc2lnbmVkSW50MzIsIHVuc2lnbmVkSW50OCwgdW5zaWduZWRJbnQxNiwgdW5zaWduZWRJbnQzMlxuXG4jIyBGbG9hdHNcbkBkb2NzIGZsb2F0MzIsIGZsb2F0NjRcblxuIyMgQnl0ZXNcbkBkb2NzIGJ5dGVzXG5cbi19XG5cblxuaW1wb3J0IEFycmF5IGV4cG9zaW5nIChBcnJheSlcbmltcG9ydCBCYXNpY3MgZXhwb3NpbmcgKC4uKVxuaW1wb3J0IEJ5dGVzIGV4cG9zaW5nIChCeXRlcywgRW5kaWFubmVzcyguLikpXG5pbXBvcnQgTWF5YmUgZXhwb3NpbmcgKE1heWJlKC4uKSlcbmltcG9ydCBTdHJpbmcgZXhwb3NpbmcgKFN0cmluZylcblxuXG5cbi0tIEVOQ09ERVJcblxuXG57LXwgRGVzY3JpYmVzIGhvdyB0byBnZW5lcmF0ZSBhIHNlcXVlbmNlIG9mIGJ5dGVzLlxuXG5UaGVzZSBlbmNvZGVycyBzbmFwIHRvZ2V0aGVyIHdpdGggW2BzZXF1ZW5jZWBdKCNzZXF1ZW5jZSkgc28geW91IGNhbiBzdGFydCB3aXRoXG5zbWFsbCBidWlsZGluZyBibG9ja3MgYW5kIHB1dCB0aGVtIHRvZ2V0aGVyIGludG8gYSBtb3JlIGNvbXBsZXggZW5jb2RpbmcuXG4tfVxudHlwZSBFbmNvZGVyXG4gID0gSTggSW50XG4gIHwgSTE2IEVuZGlhbm5lc3MgSW50XG4gIHwgSTMyIEVuZGlhbm5lc3MgSW50XG4gIHwgVTggSW50XG4gIHwgVTE2IEVuZGlhbm5lc3MgSW50XG4gIHwgVTMyIEVuZGlhbm5lc3MgSW50XG4gIHwgRjMyIEVuZGlhbm5lc3MgRmxvYXRcbiAgfCBGNjQgRW5kaWFubmVzcyBGbG9hdFxuICB8IFNlcSBJbnQgKEFycmF5IEVuY29kZXIpXG4gIHwgVXRmOCBJbnQgU3RyaW5nXG4gIHwgQnl0ZXMgQnl0ZXNcblxuXG5cbi0tIEVOQ09ERVxuXG5cbnstfCBUdXJuIGFuIGBFbmNvZGVyYCBpbnRvIGBCeXRlc2AuXG5cbiAgICBlbmNvZGUgKHVuc2lnbmVkSW50OCAgICAgNykgLS0gPDA3PlxuICAgIGVuY29kZSAodW5zaWduZWRJbnQxNiBCRSA3KSAtLSA8MDAwNz5cbiAgICBlbmNvZGUgKHVuc2lnbmVkSW50MTYgTEUgNykgLS0gPDA3MDA+XG5cblRoZSBgZW5jb2RlYCBmdW5jdGlvbiBpcyBkZXNpZ25lZCB0byBtaW5pbWl6ZSBhbGxvY2F0aW9uLiBJdCBmaWd1cmVzIG91dCB0aGVcbmV4YWN0IGxlbmd0aCBuZWNlc3NhcnkgdG8gZml0IGV2ZXJ5dGhpbmcgaW4gYEJ5dGVzYCBhbmQgdGhlbiBnZW5lcmF0ZSB0aGF0XG52YWx1ZSBkaXJlY3RseS4gVGhpcyBpcyB2YWx1YWJsZSB3aGVuIHlvdSBhcmUgZW5jb2RpbmcgbW9yZSBlbGFib3JhdGUgZGF0YTpcblxuICAgIGltcG9ydCBCeXRlcyBleHBvc2luZyAoRW5kaWFubmVzcyguLikpXG4gICAgaW1wb3J0IEJ5dGVzLkVuY29kZSBhcyBFbmNvZGVcblxuICAgIHR5cGUgYWxpYXMgUGVyc29uID1cbiAgICAgIHsgYWdlIDogSW50XG4gICAgICAsIG5hbWUgOiBTdHJpbmdcbiAgICAgIH1cblxuICAgIHRvRW5jb2RlciA6IFBlcnNvbiAtPiBFbmNvZGUuRW5jb2RlclxuICAgIHRvRW5jb2RlciBwZXJzb24gPVxuICAgICAgRW5jb2RlLnNlcXVlbmNlXG4gICAgICAgIFsgRW5jb2RlLnVuc2lnbmVkSW50MTYgQkUgcGVyc29uLmFnZVxuICAgICAgICAsIEVuY29kZS51bnNpZ25lZEludDE2IEJFIChFbmNvZGUuZ2V0U3RyaW5nV2lkdGggcGVyc29uLm5hbWUpXG4gICAgICAgICwgRW5jb2RlLnN0cmluZyBwZXJzb24ubmFtZVxuICAgICAgICBdXG5cbiAgICAtLSBlbmNvZGUgKHRvRW5jb2RlciAoeyBhZ2UgPSAzMywgbmFtZSA9IFwiVG9tXCIgfSkpID09IDwwMDIxMDAwMzU0NkY2RD5cblxuRGlkIHlvdSBrbm93IGl0IHdhcyBnb2luZyB0byBiZSBzZXZlbiBieXRlcz8gSG93IGFib3V0IHdoZW4geW91IGhhdmUgYSBodW5kcmVkXG5wZW9wbGUgdG8gc2VyaWFsaXplPyBBbmQgd2hlbiBzb21lIGhhdmUgSmFwYW5lc2UgYW5kIE5vcndlZ2lhbiBuYW1lcz8gSGF2aW5nXG50aGlzIGludGVybWVkaWF0ZSBgRW5jb2RlcmAgY2FuIGhlbHAgcmVkdWNlIGFsbG9jYXRpb24gcXVpdGUgYSBsb3QhXG4tfVxuZW5jb2RlIDogRW5jb2RlciAtPiBCeXRlc1xuZW5jb2RlID1cbiAgR3Jlbi5LZXJuZWwuQnl0ZXMuZW5jb2RlXG5cblxuXG4tLSBJTlRFR0VSU1xuXG5cbnstfCBFbmNvZGUgaW50ZWdlcnMgZnJvbSBgLTEyOGAgdG8gYDEyN2AgaW4gb25lIGJ5dGUuXG4tfVxuc2lnbmVkSW50OCA6IEludCAtPiBFbmNvZGVyXG5zaWduZWRJbnQ4ID1cbiAgSThcblxuXG57LXwgRW5jb2RlIGludGVnZXJzIGZyb20gYC0zMjc2OGAgdG8gYDMyNzY3YCBpbiB0d28gYnl0ZXMuXG4tfVxuc2lnbmVkSW50MTYgOiBFbmRpYW5uZXNzIC0+IEludCAtPiBFbmNvZGVyXG5zaWduZWRJbnQxNiA9XG4gIEkxNlxuXG5cbnstfCBFbmNvZGUgaW50ZWdlcnMgZnJvbSBgLTIxNDc0ODM2NDhgIHRvIGAyMTQ3NDgzNjQ3YCBpbiBmb3VyIGJ5dGVzLlxuLX1cbnNpZ25lZEludDMyIDogRW5kaWFubmVzcyAtPiBJbnQgLT4gRW5jb2Rlclxuc2lnbmVkSW50MzIgPVxuICBJMzJcblxuXG57LXwgRW5jb2RlIGludGVnZXJzIGZyb20gYDBgIHRvIGAyNTVgIGluIG9uZSBieXRlLlxuLX1cbnVuc2lnbmVkSW50OCA6IEludCAtPiBFbmNvZGVyXG51bnNpZ25lZEludDggPVxuICBVOFxuXG5cbnstfCBFbmNvZGUgaW50ZWdlcnMgZnJvbSBgMGAgdG8gYDY1NTM1YCBpbiB0d28gYnl0ZXMuXG4tfVxudW5zaWduZWRJbnQxNiA6IEVuZGlhbm5lc3MgLT4gSW50IC0+IEVuY29kZXJcbnVuc2lnbmVkSW50MTYgPVxuICBVMTZcblxuXG57LXwgRW5jb2RlIGludGVnZXJzIGZyb20gYDBgIHRvIGA0Mjk0OTY3Mjk1YCBpbiBmb3VyIGJ5dGVzLlxuLX1cbnVuc2lnbmVkSW50MzIgOiBFbmRpYW5uZXNzIC0+IEludCAtPiBFbmNvZGVyXG51bnNpZ25lZEludDMyID1cbiAgVTMyXG5cblxuXG4tLSBGTE9BVFNcblxuXG57LXwgRW5jb2RlIDMyLWJpdCBmbG9hdGluZyBwb2ludCBudW1iZXJzIGluIGZvdXIgYnl0ZXMuXG4tfVxuZmxvYXQzMiA6IEVuZGlhbm5lc3MgLT4gRmxvYXQgLT4gRW5jb2RlclxuZmxvYXQzMiA9XG4gIEYzMlxuXG5cbnstfCBFbmNvZGUgNjQtYml0IGZsb2F0aW5nIHBvaW50IG51bWJlcnMgaW4gZWlnaHQgYnl0ZXMuXG4tfVxuZmxvYXQ2NCA6IEVuZGlhbm5lc3MgLT4gRmxvYXQgLT4gRW5jb2RlclxuZmxvYXQ2NCA9XG4gIEY2NFxuXG5cblxuLS0gQllURVNcblxuXG57LXwgQ29weSBieXRlcyBkaXJlY3RseSBpbnRvIHRoZSBuZXcgYEJ5dGVzYCBzZXF1ZW5jZS4gVGhpcyBkb2VzIG5vdCByZWNvcmQgdGhlXG5sZW5ndGggdGhvdWdoISBZb3UgdXN1YWxseSB3YW50IHRvIHNheSBzb21ldGhpbmcgbGlrZSB0aGlzOlxuXG4gICAgaW1wb3J0IEJ5dGVzIGV4cG9zaW5nIChCeXRlcywgRW5kaWFubmVzcyguLikpXG4gICAgaW1wb3J0IEJ5dGVzLkVuY29kZSBhcyBFbmNvZGVcblxuICAgIHBuZyA6IEJ5dGVzIC0+IEVuY29kZS5FbmNvZGVyXG4gICAgcG5nIGltYWdlRGF0YSA9XG4gICAgICBFbmNvZGUuc2VxdWVuY2VcbiAgICAgICAgWyBFbmNvZGUudW5zaWduZWRJbnQzMiBCRSAoQnl0ZXMubGVuZ3RoIGltYWdlRGF0YSlcbiAgICAgICAgLCBFbmNvZGUuYnl0ZXMgaW1hZ2VEYXRhXG4gICAgICAgIF1cblxuVGhpcyBhbGxvd3MgeW91IHRvIHJlcHJlc2VudCB0aGUgbGVuZ3RoIGhvd2V2ZXIgaXMgbmVjZXNzYXJ5IGZvciB5b3VyIHByb3RvY29sLlxuRm9yIGV4YW1wbGUsIHlvdSBjYW4gdXNlIFtCYXNlIDEyOCBWYXJpbnRzXVtwYl0gZm9yIFByb3RvQnVmLFxuW1ZhcmlhYmxlLUxlbmd0aCBJbnRlZ2Vyc11bc3FsXSBmb3IgU1FMaXRlLCBvciB3aGF0ZXZlciBlbHNlIHRoZXkgZHJlYW0gdXAuXG5cbltwYl06IGh0dHBzOi8vZGV2ZWxvcGVycy5nb29nbGUuY29tL3Byb3RvY29sLWJ1ZmZlcnMvZG9jcy9lbmNvZGluZyN2YXJpbnRzXG5bc3FsXTogaHR0cHM6Ly93d3cuc3FsaXRlLm9yZy9zcmM0L2RvYy90cnVuay93d3cvdmFyaW50Lndpa2lcbi19XG5ieXRlcyA6IEJ5dGVzIC0+IEVuY29kZXJcbmJ5dGVzID1cbiAgQnl0ZXNcblxuXG4tLSBTRVFVRU5DRVxuXG5cbnstfCBQdXQgdG9nZXRoZXIgYSBidW5jaCBvZiBidWlsZGVycy4gU28gaWYgeW91IHdhbnRlZCB0byBlbmNvZGUgdGhyZWUgYEZsb2F0YFxudmFsdWVzIGZvciB0aGUgcG9zaXRpb24gb2YgYSBiYWxsIGluIDNEIHNwYWNlLCB5b3UgY291bGQgc2F5OlxuXG4gICAgaW1wb3J0IEJ5dGVzIGV4cG9zaW5nIChFbmRpYW5uZXNzKC4uKSlcbiAgICBpbXBvcnQgQnl0ZXMuRW5jb2RlIGFzIEVuY29kZVxuXG4gICAgdHlwZSBhbGlhcyBCYWxsID0geyB4IDogRmxvYXQsIHkgOiBGbG9hdCwgeiA6IEZsb2F0IH1cblxuICAgIGJhbGwgOiBCYWxsIC0+IEVuY29kZS5FbmNvZGVyXG4gICAgYmFsbCB7eCx5LHp9ID1cbiAgICAgIEVuY29kZS5zZXF1ZW5jZVxuICAgICAgICBbIEVuY29kZS5mbG9hdDMyIEJFIHhcbiAgICAgICAgLCBFbmNvZGUuZmxvYXQzMiBCRSB5XG4gICAgICAgICwgRW5jb2RlLmZsb2F0MzIgQkUgelxuICAgICAgICBdXG5cbi19XG5zZXF1ZW5jZSA6IEFycmF5IEVuY29kZXIgLT4gRW5jb2Rlclxuc2VxdWVuY2UgYnVpbGRlcnMgPVxuICBTZXEgKGdldExlbmd0aHMgMCBidWlsZGVycykgYnVpbGRlcnNcblxuXG4tLSBXUklURVxuXG5cbndyaXRlIDogRW5jb2RlciAtPiBCeXRlcyAtPiBJbnQgLT4gSW50XG53cml0ZSBidWlsZGVyIG1iIG9mZnNldCA9XG4gIGNhc2UgYnVpbGRlciBvZlxuICAgIEk4ICAgIG4gLT4gR3Jlbi5LZXJuZWwuQnl0ZXMud3JpdGVfaTggIG1iIG9mZnNldCBuXG4gICAgSTE2IGUgbiAtPiBHcmVuLktlcm5lbC5CeXRlcy53cml0ZV9pMTYgbWIgb2Zmc2V0IG4gKGUgPT0gTEUpXG4gICAgSTMyIGUgbiAtPiBHcmVuLktlcm5lbC5CeXRlcy53cml0ZV9pMzIgbWIgb2Zmc2V0IG4gKGUgPT0gTEUpXG4gICAgVTggICAgbiAtPiBHcmVuLktlcm5lbC5CeXRlcy53cml0ZV91OCAgbWIgb2Zmc2V0IG5cbiAgICBVMTYgZSBuIC0+IEdyZW4uS2VybmVsLkJ5dGVzLndyaXRlX3UxNiBtYiBvZmZzZXQgbiAoZSA9PSBMRSlcbiAgICBVMzIgZSBuIC0+IEdyZW4uS2VybmVsLkJ5dGVzLndyaXRlX3UzMiBtYiBvZmZzZXQgbiAoZSA9PSBMRSlcbiAgICBGMzIgZSBuIC0+IEdyZW4uS2VybmVsLkJ5dGVzLndyaXRlX2YzMiBtYiBvZmZzZXQgbiAoZSA9PSBMRSlcbiAgICBGNjQgZSBuIC0+IEdyZW4uS2VybmVsLkJ5dGVzLndyaXRlX2Y2NCBtYiBvZmZzZXQgbiAoZSA9PSBMRSlcbiAgICBTZXEgXyBicyAtPiB3cml0ZVNlcXVlbmNlIGJzIG1iIG9mZnNldFxuICAgIFV0ZjggXyBzIC0+IEdyZW4uS2VybmVsLkJ5dGVzLndyaXRlX3N0cmluZyBtYiBvZmZzZXQgc1xuICAgIEJ5dGVzIGJzIC0+IEdyZW4uS2VybmVsLkJ5dGVzLndyaXRlX2J5dGVzIG1iIG9mZnNldCBic1xuXG5cbndyaXRlU2VxdWVuY2UgOiBBcnJheSBFbmNvZGVyIC0+IEJ5dGVzIC0+IEludCAtPiBJbnRcbndyaXRlU2VxdWVuY2UgYnVpbGRlcnMgbWIgb2Zmc2V0ID1cbiAgQXJyYXkuZm9sZGxcbiAgICAoXFxidWlsZGVyIGN1cnJlbnRPZmZzZXQgLT5cbiAgICAgIHdyaXRlIGJ1aWxkZXIgbWIgY3VycmVudE9mZnNldFxuICAgIClcbiAgICBvZmZzZXRcbiAgICBidWlsZGVyc1xuXG5cbi0tIExFTkdUSFNcblxuXG5nZXRMZW5ndGggOiBFbmNvZGVyIC0+IEludFxuZ2V0TGVuZ3RoIGJ1aWxkZXIgPVxuICBjYXNlIGJ1aWxkZXIgb2ZcbiAgICBJOCAgICBfIC0+IDFcbiAgICBJMTYgXyBfIC0+IDJcbiAgICBJMzIgXyBfIC0+IDRcbiAgICBVOCAgICBfIC0+IDFcbiAgICBVMTYgXyBfIC0+IDJcbiAgICBVMzIgXyBfIC0+IDRcbiAgICBGMzIgXyBfIC0+IDRcbiAgICBGNjQgXyBfIC0+IDhcbiAgICBTZXEgdyBfIC0+IHdcbiAgICBVdGY4IHcgXyAtPiB3XG4gICAgQnl0ZXMgYnMgLT4gR3Jlbi5LZXJuZWwuQnl0ZXMubGVuZ3RoIGJzXG5cblxuZ2V0TGVuZ3RocyA6IEludCAtPiBBcnJheSBFbmNvZGVyIC0+IEludFxuZ2V0TGVuZ3RocyBsZW5ndGggYnVpbGRlcnMgPVxuICBBcnJheS5mb2xkbCBcbiAgICAoXFxidWlsZGVyIHN1bSAtPlxuICAgICAgc3VtICsgZ2V0TGVuZ3RoIGJ1aWxkZXJcbiAgICApXG4gICAgbGVuZ3RoXG4gICAgYnVpbGRlcnNcbiIsCiAgICAgICAgIm1vZHVsZSBCeXRlcyBleHBvc2luZ1xuICAoIEJ5dGVzXG4gICwgZW1wdHlcbiAgLCBpc0VtcHR5XG4gICwgbGVuZ3RoXG4gIC0tXG4gICwgRW5kaWFubmVzcyguLilcbiAgLCBnZXRIb3N0RW5kaWFubmVzc1xuICAtLVxuICAsIGZyb21TdHJpbmdcbiAgLCB0b1N0cmluZ1xuICAtLVxuICAsIGpvaW5cbiAgKVxuXG5cbnstfCBGdW5jdGlvbnMgZm9yIHdvcmtpbmcgd2l0aCBzZXF1ZW5jZXMgb2YgYnl0ZXMuXG5cbkBkb2NzIEJ5dGVzLCBlbXB0eSwgaXNFbXB0eSwgbGVuZ3RoXG5cbiMjIEVuZGlhbm5lc3NcbkBkb2NzIEVuZGlhbm5lc3MsIGdldEhvc3RFbmRpYW5uZXNzXG5cbiMjIFN0cmluZ3NcbkBkb2NzIGZyb21TdHJpbmcsIHRvU3RyaW5nXG5cbiMjIENvbWJpbmVcbkBkb2NzIGpvaW5cblxuLX1cblxuXG5pbXBvcnQgQXJyYXkgZXhwb3NpbmcgKEFycmF5KVxuaW1wb3J0IEJhc2ljcyBleHBvc2luZyAoLi4pXG5pbXBvcnQgTWF5YmUgZXhwb3NpbmcgKE1heWJlKVxuaW1wb3J0IFN0cmluZyBleHBvc2luZyAoU3RyaW5nKVxuaW1wb3J0IFRhc2sgZXhwb3NpbmcgKFRhc2spXG5pbXBvcnQgR3Jlbi5LZXJuZWwuQnl0ZXNcblxuXG4tLSBCWVRFU1xuXG5cbnstfCBBIHNlcXVlbmNlIG9mIGJ5dGVzLlxuXG5BIGJ5dGUgaXMgYSBjaHVuayBvZiBlaWdodCBiaXRzLiBGb3IgZXhhbXBsZSwgdGhlIGxldHRlciBgamAgaXMgdXN1YWxseVxucmVwcmVzZW50ZWQgYXMgdGhlIGJ5dGUgYDAxMTAxMDEwYCwgYW5kIHRoZSBsZXR0ZXIgYGtgIGlzIGAwMTEwMTAxMWAuXG5cblNlZWluZyBlYWNoIGJ5dGUgYXMgYSBzdHJlYW0gb2YgemVyb3MgYW5kIG9uZXMgY2FuIGJlIHF1aXRlIGNvbmZ1c2luZyB0aG91Z2gsXG5zbyBpdCBpcyBjb21tb24gdG8gdXNlIGhleGlkZWNpbWFsIG51bWJlcnMgaW5zdGVhZDpcblxuYGBgXG58IEJpbmFyeSB8IEhleCB8XG4rLS0tLS0tLS0rLS0tLS0rXG58ICAwMDAwICB8ICAwICB8XG58ICAwMDAxICB8ICAxICB8XG58ICAwMDEwICB8ICAyICB8XG58ICAwMDExICB8ICAzICB8ICAgICBqID0gMDExMDEwMTBcbnwgIDAxMDAgIHwgIDQgIHwgICAgICAgICBcXF9fL1xcX18vXG58ICAwMTAxICB8ICA1ICB8ICAgICAgICAgICB8ICAgfFxufCAgMDExMCAgfCAgNiAgfCAgICAgICAgICAgNiAgIEFcbnwgIDAxMTEgIHwgIDcgIHxcbnwgIDEwMDAgIHwgIDggIHwgICAgIGsgPSAwMTEwMTAxMVxufCAgMTAwMSAgfCAgOSAgfCAgICAgICAgIFxcX18vXFxfXy9cbnwgIDEwMTAgIHwgIEEgIHwgICAgICAgICAgIHwgICB8XG58ICAxMDExICB8ICBCICB8ICAgICAgICAgICA2ICAgQlxufCAgMTEwMCAgfCAgQyAgfFxufCAgMTEwMSAgfCAgRCAgfFxufCAgMTExMCAgfCAgRSAgfFxufCAgMTExMSAgfCAgRiAgfFxuYGBgXG5cblNvIGBqYCBpcyBgNkFgIGFuZCBga2AgaXMgYDZCYCBpbiBoZXhpZGVjaW1hbC4gVGhpcyBtb3JlIGNvbXBhY3QgcmVwcmVzZW50YXRpb25cbmlzIGdyZWF0IHdoZW4geW91IGhhdmUgYSBzZXF1ZW5jZSBvZiBieXRlcy4gWW91IGNhbiBzZWUgdGhpcyBldmVuIGluIGEgc2hvcnRcbnN0cmluZyBsaWtlIGBcImphenpcImA6XG5cbmBgYFxuYmluYXJ5ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgaGV4aWRlY2ltYWxcbjAxMTAxMDEwIDAxMTAwMDAxIDAxMTExMDEwIDAxMTExMDEwID0+IDZBIDYxIDdBIDdBXG5gYGBcblxuQW55d2F5LCB0aGUgcG9pbnQgaXMgdGhhdCBgQnl0ZXNgIGlzIGEgc2VxdWVuY2Ugb2YgYnl0ZXMhXG4tfVxudHlwZSBCeXRlcyA9IEJ5dGVzXG5cblxuey18IFRoZSBzZXF1ZW5jZSB3aXRoIGV4YWN0bHkgMCBieXRlcy5cbi19XG5lbXB0eSA6IEJ5dGVzXG5lbXB0eSA9XG4gIEdyZW4uS2VybmVsLkJ5dGVzLmVtcHR5XG5cblxuey18IENoZWNrIGlmIHRoZSBnaXZlbiBieXRlIHNlcXVlbmNlIGhhcyAwIGJ5dGVzIGluIGl0LlxuLX1cbmlzRW1wdHkgOiBCeXRlcyAtPiBCb29sXG5pc0VtcHR5IGJ5dGVzID1cbiAgbGVuZ3RoIGJ5dGVzID09IDBcblxuXG57LXwgR2V0IHRoZSBsZW5ndGggb2YgYSBzZXF1ZW5jZSBvZiBieXRlcy5cblxuU28gaWYgYSBzZXF1ZW5jZSBoYXMgZm91ci1odW5kcmVkIGJ5dGVzLCB0aGVuIGBsZW5ndGggYnl0ZXNgIHdvdWxkIGdpdmUgYmFja1xuYDQwMGAuIFRoYXQgbWF5IGJlIDQwMCB1bnNpZ25lZCA4LWJpdCBpbnRlZ2VycywgMTAwIHNpZ25lZCAzMi1iaXQgaW50ZWdlcnMsIG9yXG5ldmVuIGEgVVRGLTggc3RyaW5nLiBUaGUgY29udGVudCBkb2VzIG5vdCBtYXR0ZXIuIFRoaXMgaXMganVzdCBmaWd1cmluZyBvdXRcbmhvdyBtYW55IGJ5dGVzIHRoZXJlIGFyZSFcbi19XG5sZW5ndGggOiBCeXRlcyAtPiBJbnRcbmxlbmd0aCA9XG4gIEdyZW4uS2VybmVsLkJ5dGVzLmxlbmd0aFxuXG5cblxuLS0gRU5ESUFOTkVTU1xuXG5cbnstfCBEaWZmZXJlbnQgY29tcHV0ZXJzIHN0b3JlIGludGVnZXJzIGFuZCBmbG9hdHMgc2xpZ2h0bHkgZGlmZmVyZW50bHkgaW5cbm1lbW9yeS4gU2F5IHdlIGhhdmUgdGhlIGludGVnZXIgYDB4MUEyQjNDNERgIGluIG91ciBwcm9ncmFtLiBJdCBuZWVkcyBmb3VyXG5ieXRlcyAoMzIgYml0cykgaW4gbWVtb3J5LiBJdCBtYXkgc2VlbSByZWFzb25hYmxlIHRvIGxheSB0aGVtIG91dCBpbiBvcmRlcjpcblxuYGBgXG4gICBCaWctRW5kaWFuIChCRSkgICAgICAoT2J2aW91cyBPcmRlcilcbistLS0tKy0tLS0rLS0tLSstLS0tK1xufCAxQSB8IDJCIHwgM0MgfCA0RCB8XG4rLS0tLSstLS0tKy0tLS0rLS0tLStcbmBgYFxuXG5CdXQgc29tZSBwZW9wbGUgdGhvdWdodCBpdCB3b3VsZCBiZSBiZXR0ZXIgdG8gc3RvcmUgdGhlIGJ5dGVzIGluIHRoZSBvcHBvc2l0ZVxub3JkZXI6XG5cbmBgYFxuICBMaXR0bGUtRW5kaWFuIChMRSkgICAgKFNodWZmbGVkIE9yZGVyKVxuKy0tLS0rLS0tLSstLS0tKy0tLS0rXG58IDREIHwgM0MgfCAyQiB8IDFBIHxcbistLS0tKy0tLS0rLS0tLSstLS0tK1xuYGBgXG5cbk5vdGljZSB0aGF0ICoqdGhlIF9ieXRlc18gYXJlIHNodWZmbGVkLCBub3QgdGhlIGJpdHMuKiogSXQgaXMgbGlrZSBpZiB5b3UgY3V0IGFcbnBob3RvIGludG8gZm91ciBzdHJpcHMgYW5kIHNodWZmbGVkIHRoZSBzdHJpcHMuIEl0IGlzIG5vdCBhIG1pcnJvciBpbWFnZS5cblRoZSB0aGVvcnkgc2VlbXMgdG8gYmUgdGhhdCBhbiA4LWJpdCBgMHgxQWAgYW5kIGEgMzItYml0IGAweDAwMDAwMDFBYCBib3RoIGhhdmVcbmAxQWAgYXMgdGhlIGZpcnN0IGJ5dGUgaW4gdGhpcyBzY2hlbWUuIE1heWJlIHRoaXMgd2FzIGhlbHBmdWwgd2hlbiBwcm9jZXNzb3JzXG5oYW5kbGVkIG9uZSBieXRlIGF0IGEgdGltZS5cblxuKipNb3N0IHByb2Nlc3NvcnMgdXNlIGxpdHRsZS1lbmRpYW4gKExFKSBsYXlvdXQuKiogVGhpcyBzZWVtcyB0byBiZSBiZWNhdXNlXG5JbnRlbCBkaWQgaXQgdGhpcyB3YXksIGFuZCBvdGhlciBjaGlwIG1hbnVmYWN0dXJlcyBmb2xsb3dlZCB0aGVpciBjb252ZW50aW9uLlxuKipNb3N0IG5ldHdvcmsgcHJvdG9jb2xzIHVzZSBiaWctZW5kaWFuIChCRSkgbGF5b3V0LioqIEkgc3VzcGVjdCB0aGlzIGlzXG5iZWNhdXNlIGlmIHlvdSBhcmUgdHJ5aW5nIHRvIGRlYnVnIGEgbmV0d29yayBwcm90b2NvbCwgaXQgaXMgbmljZSBpZiB5b3VyXG5pbnRlZ2VycyBhcmUgbm90IGFsbCBzaHVmZmxlZC5cblxuKipOb3RlOioqIEVuZGlhbm5lc3MgaXMgcmVsZXZhbnQgZm9yIGludGVnZXJzIGFuZCBmbG9hdHMsIGJ1dCBub3Qgc3RyaW5ncy5cblVURi04IHNwZWNpZmllcyB0aGUgb3JkZXIgb2YgYnl0ZXMgZXhwbGljaXRseS5cblxuKipOb3RlOioqIFRoZSB0ZXJtcyBsaXR0bGUtZW5kaWFuIGFuZCBiaWctZW5kaWFuIGFyZSBhIHJlZmVyZW5jZSB0byBhbiBlZ2cgam9rZVxuaW4gR3VsbGl2ZXIncyBUcmF2ZWxzLiBUaGV5IGZpcnN0IGFwcGVhcmVkIGluIDE5ODAgaW4gW3RoaXMgZXNzYXldW2Vzc2F5XSwgYW5kXG55b3UgY2FuIGRlY2lkZSBmb3IgeW91cnNlbGYgaWYgdGhleSBzdG9vZCB0aGUgdGVzdCBvZiB0aW1lLiBJIHBlcnNvbmFsbHkgZmluZFxudGhlc2UgdGVybXMgcXVpdGUgdW5oZWxwZnVsLCBzbyBJIHNheSDigJxPYnZpb3VzIE9yZGVy4oCdIGFuZCDigJxTaHVmZmxlZCBPcmRlcuKAncKgaW5cbm15IGhlYWQuIEkgcmVtZW1iZXIgd2hpY2ggaXMgbW9yZSBjb21tb24gYnkgYXNraW5nIG15c2VsZiwg4oCcaWYgdGhpbmdzIHdlcmVcbm9idmlvdXMsIHdvdWxkIEkgaGF2ZSB0byBhc2sgdGhpcyBxdWVzdGlvbj/igJ1cblxuW2Vzc2F5XTogaHR0cDovL3d3dy5pZXRmLm9yZy9yZmMvaWVuL2llbjEzNy50eHRcbi19XG50eXBlIEVuZGlhbm5lc3MgPSBMRSB8IEJFXG5cblxuey18IElzIHRoaXMgcHJvZ3JhbSBydW5uaW5nIG9uIGEgYmlnLWVuZGlhbiBvciBsaXR0bGUtZW5kaWFuIG1hY2hpbmU/XG4tfVxuZ2V0SG9zdEVuZGlhbm5lc3MgOiBUYXNrIHggRW5kaWFubmVzc1xuZ2V0SG9zdEVuZGlhbm5lc3MgPVxuICBHcmVuLktlcm5lbC5CeXRlcy5nZXRIb3N0RW5kaWFubmVzcyBMRSBCRVxuXG5cbi0tIFNUUklOR1NcblxuXG57LXwgQ29udmVydCBhIGBTdHJpbmdgIHRvIGEgYEJ5dGVzYC4gVGhlIHJlc3VsdGluZyBieXRlcyB3aWxsIGJlIGluIHRoZVxuVVRGLTggZW5jb2RpbmcuXG5cblNvbWUgY2hhcmFjdGVycyB0YWtlIG9uZSBieXRlLCB3aGlsZSBvdGhlcnMgY2FuIHRha2UgdXAgdG8gZm91ci4gUmVhZCBtb3JlXG5hYm91dCBbVVRGLThdKGh0dHBzOi8vZW4ud2lraXBlZGlhLm9yZy93aWtpL1VURi04KSB0byBsZWFybiB0aGUgZGV0YWlscyFcbi19XG5mcm9tU3RyaW5nIDogU3RyaW5nIC0+IEJ5dGVzXG5mcm9tU3RyaW5nID1cbiAgR3Jlbi5LZXJuZWwuQnl0ZXMuZnJvbVN0cmluZ1xuXG5cbnstfCBDb252ZXJ0IFVURi04IGVuY29kZWQgYEJ5dGVzYCB0byBgU3RyaW5nYC4gSWYgdGhlIGJ5dGUgc2VxdWVuY2UgaXNuJ3RcbnZhbGlkIFVURi04LCBgTm90aGluZ2Agd2lsbCBiZSByZXR1cm5lZC5cbi19XG50b1N0cmluZyA6IEJ5dGVzIC0+IE1heWJlIFN0cmluZ1xudG9TdHJpbmcgPVxuICBHcmVuLktlcm5lbC5CeXRlcy50b1N0cmluZ1xuXG5cbi0tIENPTUJJTkVcblxuXG57LXwgSm9pbiBhbGwgYEJ5dGVzYCBpbiBhbiBgQXJyYXlgIGludG8gYSBzaW5nbGUgYEJ5dGVzYC5cbi19XG5qb2luIDogQXJyYXkgQnl0ZXMgLT4gQnl0ZXNcbmpvaW4gPVxuICBHcmVuLktlcm5lbC5CeXRlcy5qb2luXG4iLAogICAgICAgICJlZmZlY3QgbW9kdWxlIFN0cmVhbSB3aGVyZSB7IHN1YnNjcmlwdGlvbiA9IFN0cmVhbVN1YiB9IGV4cG9zaW5nXG4gICAgKCBTdHJlYW1cbiAgICAsIHNlbmRcbiAgICAsIHNlbmRTdHJpbmdcbiAgICAsIHNlbmRMaW5lXG4gICAgLCBsaXN0ZW5cbiAgICApXG5cbnstfCBBIGBTdHJlYW1gIGlzIGFuIGFic3RyYWN0IGZsb3cgb2YgZGF0YSBmcm9tIG9uZSBwb2ludCB0byBhbm90aGVyLFxubGlrZSBieXRlcyBmbG93aW5nIGZyb20gYSBzZXJ2ZXIgdG8gYSBjbGllbnQsIG9yIGZyb20gYSBmaWxlIHRvIGEgcHJvZ3JhbS5cblxuQGRvY3MgU3RyZWFtLCBzZW5kLCBzZW5kU3RyaW5nLCBzZW5kTGluZSwgbGlzdGVuXG5cbi19XG5cbmltcG9ydCBEaWN0IGV4cG9zaW5nIChEaWN0KVxuaW1wb3J0IEdyZW4uS2VybmVsLlN0cmVhbVxuaW1wb3J0IFRhc2sgZXhwb3NpbmcgKFRhc2spXG5pbXBvcnQgQnl0ZXMgZXhwb3NpbmcgKEJ5dGVzKVxuaW1wb3J0IFByb2Nlc3NcbmltcG9ydCBJbnRlcm5hbC5TdHJlYW0gYXMgSW50ZXJuYWxcblxuXG57LXwgQSBzcGVjaWZpYyBzdHJlYW0uXG4tfVxudHlwZSBhbGlhcyBTdHJlYW1cbiAgICA9IEludGVybmFsLlN0cmVhbVxuXG5cbnstfCBUaGlzIGxldHMgeW91IHNlbmQgYnl0ZXMgb3ZlciBhIGBTdHJlYW1gLlxuLX1cbnNlbmQgOiBTdHJlYW0gLT4gQnl0ZXMgLT4gVGFzayB4IHt9XG5zZW5kIChJbnRlcm5hbC5TdHJlYW0gXyBrZXJuZWxTdHJlYW0pIGJ5dGVzID1cbiAgICBHcmVuLktlcm5lbC5TdHJlYW0uc2VuZCBrZXJuZWxTdHJlYW0gYnl0ZXNcblxuXG57LXwgVGhpcyBsZXRzIHlvdSBzZW5kIGEgc3RyaW5nIG92ZXIgYSBgU3RyZWFtYC5cbi19XG5zZW5kU3RyaW5nIDogU3RyZWFtIC0+IFN0cmluZyAtPiBUYXNrIHgge31cbnNlbmRTdHJpbmcgc3RyZWFtIHN0cmluZyA9XG4gICAgc2VuZCBzdHJlYW0gPHwgQnl0ZXMuZnJvbVN0cmluZyBzdHJpbmdcblxuXG57LXwgVGhpcyBzZW5kcyBzdHJpbmcgYW5kIGEgbmV3bGluZSBvdmVyIGEgYFN0cmVhbWAuXG4tfVxuc2VuZExpbmUgOiBTdHJlYW0gLT4gU3RyaW5nIC0+IFRhc2sgeCB7fVxuc2VuZExpbmUgc3RyZWFtIHN0cmluZyA9XG4gICAgc2VuZFN0cmluZyBzdHJlYW0gPHwgc3RyaW5nICsrIFwiXFxuXCJcblxuXG4tLSBTVUJTQ1JJUFRJT05cblxuXG50eXBlIFN0cmVhbVN1YiBtc2dcbiAgICA9IExpc3RlbiBTdHJlYW0gKEJ5dGVzIC0+IG1zZylcblxuXG5zdWJNYXAgOiAoYSAtPiBiKSAtPiBTdHJlYW1TdWIgYSAtPiBTdHJlYW1TdWIgYlxuc3ViTWFwIG1hcEZuIHN1YiA9XG4gICAgY2FzZSBzdWIgb2ZcbiAgICAgICAgTGlzdGVuIHN0cmVhbSBtc2dNYXAgLT5cbiAgICAgICAgICAgIExpc3RlbiBzdHJlYW0gKG1hcEZuIDw8IG1zZ01hcClcblxuXG57LXwgVGhpcyBub3RpZmllcyB5b3VyIGFwcGxpY2F0aW9uIGV2ZXJ5IHRpbWUgYnl0ZXMgaGF2ZSBhcnJpdmVkIG92ZXIgdGhlIGBTdHJlYW1gLlxuLX1cbmxpc3RlbiA6IFN0cmVhbSAtPiAoQnl0ZXMgLT4gbXNnKSAtPiBTdWIgbXNnXG5saXN0ZW4gc3RyZWFtIG1zZ01hcCA9XG4gICAgc3Vic2NyaXB0aW9uIChMaXN0ZW4gc3RyZWFtIG1zZ01hcClcblxuXG4tLSBMT09QXG5cblxudHlwZSBhbGlhcyBTdGF0ZSBtc2cgPVxuICAgIHsgdGFnZ2VycyA6IERpY3QgSW50IChUYWdnZXIgbXNnKVxuICAgICwgcHJvY2Vzc2VzIDogRGljdCBJbnQgUHJvY2Vzcy5JZFxuICAgIH1cblxuXG50eXBlIGFsaWFzIFRhZ2dlciBtc2cgPVxuICAgIHsgc3RyZWFtIDogU3RyZWFtXG4gICAgLCB0YWdnZXJzIDogQXJyYXkgKEJ5dGVzIC0+IG1zZylcbiAgICB9XG5cblxuaW5pdCA6IFRhc2suVGFzayBOZXZlciAoU3RhdGUgbXNnKVxuaW5pdCA9XG4gICAgVGFzay5zdWNjZWVkXG4gICAgICAgIHsgdGFnZ2VycyA9IERpY3QuZW1wdHlcbiAgICAgICAgLCBwcm9jZXNzZXMgPSBEaWN0LmVtcHR5XG4gICAgICAgIH1cblxuXG5vbkVmZmVjdHNcbiAgICA6IFBsYXRmb3JtLlJvdXRlciBtc2cgRXZlbnRcbiAgICAtPiBBcnJheSAoU3RyZWFtU3ViIG1zZylcbiAgICAtPiBTdGF0ZSBtc2dcbiAgICAtPiBUYXNrLlRhc2sgTmV2ZXIgKFN0YXRlIG1zZylcbm9uRWZmZWN0cyByb3V0ZXIgc3VicyBzdGF0ZSA9XG4gICAgbGV0XG4gICAgICAgIG5ld1RhZ2dlcnMgPVxuICAgICAgICAgICAgQXJyYXkuZm9sZGwgc3ViVG9MaXN0ZW5lciBEaWN0LmVtcHR5IHN1YnNcblxuICAgICAgICB0b1NwYXduID1cbiAgICAgICAgICAgIERpY3QuZGlmZiBuZXdUYWdnZXJzIHN0YXRlLnRhZ2dlcnNcbiAgICAgICAgICAgICAgICB8PiBEaWN0LnZhbHVlc1xuICAgICAgICAgICAgICAgIHw+IEFycmF5Lm1hcCAuc3RyZWFtXG5cbiAgICAgICAgZXhpc3RpbmdQcm9jZXNzZXMgPVxuICAgICAgICAgICAgRGljdC5maWx0ZXIgKFxcc2lkIF8gLT4gRGljdC5tZW1iZXIgc2lkIG5ld1RhZ2dlcnMpIHN0YXRlLnByb2Nlc3Nlc1xuXG4gICAgICAgIGtpbGxUYXNrcyA9XG4gICAgICAgICAgICBEaWN0LmRpZmYgc3RhdGUucHJvY2Vzc2VzIG5ld1RhZ2dlcnNcbiAgICAgICAgICAgICAgICB8PiBEaWN0LnZhbHVlc1xuICAgICAgICAgICAgICAgIHw+IEFycmF5LmZvbGRsXG4gICAgICAgICAgICAgICAgICAgIChcXGlkIHRhc2tzIC0+IFRhc2suYW5kVGhlbiAoXFx7fSAtPiBQcm9jZXNzLmtpbGwgaWQpIHRhc2tzKVxuICAgICAgICAgICAgICAgICAgICAoVGFzay5zdWNjZWVkIHt9KVxuICAgIGluXG4gICAga2lsbFRhc2tzXG4gICAgICAgIHw+IFRhc2suYW5kVGhlbiAoXFxfIC0+IHNwYXduSGVscCByb3V0ZXIgdG9TcGF3biBleGlzdGluZ1Byb2Nlc3NlcylcbiAgICAgICAgfD4gVGFzay5tYXAgKFxcbmV3UHJvY2Vzc2VzIC0+XG4gICAgICAgICAgICB7IHRhZ2dlcnMgPSBuZXdUYWdnZXJzXG4gICAgICAgICAgICAsIHByb2Nlc3NlcyA9IG5ld1Byb2Nlc3Nlc1xuICAgICAgICAgICAgfVxuICAgICAgICApXG5cblxuc3ViVG9MaXN0ZW5lciA6IFN0cmVhbVN1YiBtc2cgLT4gRGljdCBJbnQgKFRhZ2dlciBtc2cpIC0+IERpY3QgSW50IChUYWdnZXIgbXNnKVxuc3ViVG9MaXN0ZW5lciBzdWIgdGFnZ2VycyA9XG4gICAgY2FzZSBzdWIgb2ZcbiAgICAgICAgTGlzdGVuICgoSW50ZXJuYWwuU3RyZWFtIHNpZCBfKSBhcyBzdHJlYW0pIHRhZ2dlciAtPlxuICAgICAgICAgICAgY2FzZSBEaWN0LmdldCBzaWQgdGFnZ2VycyBvZlxuICAgICAgICAgICAgICAgIEp1c3QgZGF0YSAtPlxuICAgICAgICAgICAgICAgICAgICBEaWN0LnNldCBzaWRcbiAgICAgICAgICAgICAgICAgICAgICAgIHsgZGF0YSB8IHRhZ2dlcnMgPSBBcnJheS5wdXNoTGFzdCB0YWdnZXIgZGF0YS50YWdnZXJzIH1cbiAgICAgICAgICAgICAgICAgICAgICAgIHRhZ2dlcnNcblxuICAgICAgICAgICAgICAgIE5vdGhpbmcgLT5cbiAgICAgICAgICAgICAgICAgICAgRGljdC5zZXQgc2lkXG4gICAgICAgICAgICAgICAgICAgICAgICB7IHN0cmVhbSA9IHN0cmVhbVxuICAgICAgICAgICAgICAgICAgICAgICAgLCB0YWdnZXJzID0gWyB0YWdnZXIgXVxuICAgICAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgICAgICAgICAgdGFnZ2Vyc1xuXG5cbnNwYXduSGVscCA6IFBsYXRmb3JtLlJvdXRlciBtc2cgRXZlbnQgLT4gQXJyYXkgU3RyZWFtIC0+IERpY3QgSW50IFByb2Nlc3MuSWQgLT4gVGFzay5UYXNrIE5ldmVyIChEaWN0IEludCBQcm9jZXNzLklkKVxuc3Bhd25IZWxwIHJvdXRlciB0b1NwYXduIHByb2Nlc3NlcyA9XG4gIGNhc2UgQXJyYXkuZmlyc3QgdG9TcGF3biBvZlxuICAgIE5vdGhpbmcgLT5cbiAgICAgIFRhc2suc3VjY2VlZCBwcm9jZXNzZXNcblxuICAgIEp1c3QgKChJbnRlcm5hbC5TdHJlYW0gc2lkIHJhd1N0cmVhbSkgYXMgbmV4dFN0cmVhbSkgLT5cbiAgICAgIGxldFxuICAgICAgICBzcGF3blN0cmVhbSA9XG4gICAgICAgICAgUHJvY2Vzcy5zcGF3biA8fFxuICAgICAgICAgICAgYXR0YWNoTGlzdGVuZXIgcmF3U3RyZWFtIChcXGRhdGEgLT5cbiAgICAgICAgICAgICAgICBQbGF0Zm9ybS5zZW5kVG9TZWxmIHJvdXRlciAoRnJvbVN0cmVhbSBuZXh0U3RyZWFtIGRhdGEpXG4gICAgICAgICAgICApXG5cbiAgICAgICAgcmVzdCA9XG4gICAgICAgICAgQXJyYXkuZHJvcEZpcnN0IDEgdG9TcGF3blxuXG4gICAgICAgIHNwYXduUmVzdCBwcm9jZXNzSWQgPVxuICAgICAgICAgIHNwYXduSGVscCByb3V0ZXIgcmVzdCAoRGljdC5zZXQgc2lkIHByb2Nlc3NJZCBwcm9jZXNzZXMpXG4gICAgICBpblxuICAgICAgICBzcGF3blN0cmVhbVxuICAgICAgICAgIHw+IFRhc2suYW5kVGhlbiBzcGF3blJlc3RcblxuXG5hdHRhY2hMaXN0ZW5lciA6IEludGVybmFsLlJhd1N0cmVhbSAtPiAoQnl0ZXMgLT4gVGFzay5UYXNrIE5ldmVyIHt9KSAtPiBUYXNrLlRhc2sgeCBOZXZlclxuYXR0YWNoTGlzdGVuZXIgPVxuICBHcmVuLktlcm5lbC5TdHJlYW0uYXR0YWNoTGlzdGVuZXJcblxuXG50eXBlIEV2ZW50XG4gICAgPSBGcm9tU3RyZWFtIFN0cmVhbSBCeXRlc1xuXG5cbm9uU2VsZk1zZyA6IFBsYXRmb3JtLlJvdXRlciBtc2cgRXZlbnQgLT4gRXZlbnQgLT4gU3RhdGUgbXNnIC0+IFRhc2suVGFzayBOZXZlciAoU3RhdGUgbXNnKVxub25TZWxmTXNnIHJvdXRlciBldmVudCBzdGF0ZSA9XG4gICAgY2FzZSBldmVudCBvZlxuICAgICAgICBGcm9tU3RyZWFtIChJbnRlcm5hbC5TdHJlYW0gc2lkIF8pIGRhdGEgLT5cbiAgICAgICAgICAgIERpY3QuZ2V0IHNpZCBzdGF0ZS50YWdnZXJzXG4gICAgICAgICAgICAgICAgfD4gTWF5YmUubWFwIC50YWdnZXJzXG4gICAgICAgICAgICAgICAgfD4gTWF5YmUud2l0aERlZmF1bHQgW11cbiAgICAgICAgICAgICAgICB8PiBBcnJheS5tYXAgKFxcdGFnZ2VyIC0+IHRhZ2dlciBkYXRhKVxuICAgICAgICAgICAgICAgIHw+IEFycmF5LmZvbGRsXG4gICAgICAgICAgICAgICAgICAgIChcXG1zZyB0YXNrcyAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgVGFzay5hbmRUaGVuIChcXHt9IC0+IFBsYXRmb3JtLnNlbmRUb0FwcCByb3V0ZXIgbXNnKSB0YXNrc1xuICAgICAgICAgICAgICAgICAgICApXG4gICAgICAgICAgICAgICAgICAgIChUYXNrLnN1Y2NlZWQge30pXG4gICAgICAgICAgICAgICAgfD4gVGFzay5tYXAgKFxcXyAtPiBzdGF0ZSlcbiIsCiAgICAgICAgImVmZmVjdCBtb2R1bGUgSHR0cFNlcnZlciB3aGVyZSB7IHN1YnNjcmlwdGlvbiA9IEh0dHBTdWIgfSBleHBvc2luZ1xuICAgIC0tIEluaXRcbiAgICAoIFBlcm1pc3Npb25cbiAgICAsIGluaXRpYWxpemVcblxuICAgIC0tIFNlcnZlclxuICAgICwgU2VydmVyXG4gICAgLCBTZXJ2ZXJFcnJvciguLilcbiAgICAsIGNyZWF0ZVNlcnZlclxuXG4gICAgLS0gUmVxdWVzdHNcbiAgICAsIFJlcXVlc3RcbiAgICAsIE1ldGhvZCguLilcbiAgICAsIG1ldGhvZFRvU3RyaW5nXG4gICAgLCBib2R5QXNTdHJpbmdcbiAgICAsIGJvZHlGcm9tSnNvblxuICAgICwgcmVxdWVzdEluZm9cbiAgICAsIG9uUmVxdWVzdFxuICAgIClcblxuey18IENyZWF0ZSBhIHNlcnZlciB0aGF0IGNhbiByZXNwb25kIHRvIEhUVFAgcmVxdWVzdHMuXG5cbllvdSB3cml0ZSB5b3VyIHNlcnZlciB1c2luZyBUaGUgRWxtIEFyY2hpdGVjdHVyZSBieSBzdWJzY3JpYmluZyB0byByZXF1ZXN0XG5ldmVudHMgYW5kIHJlc3BvbmRpbmcgd2l0aCBjb21tYW5kcyBpbiB1cGRhdGUuXG5cblNlZSBbZXhhbXBsZXMvaHR0cC1zZXJ2ZXJdKGh0dHBzOi8vZ2l0aHViLmNvbS9ncmVuLWxhbmcvbm9kZS90cmVlL21haW4vZXhhbXBsZXMvaHR0cC1zZXJ2ZXIpIGZvciBhIHdvcmtpbmcgZXhhbXBsZS5cblxuIyMgSW5pdGlhbGl6YXRpb25cblxuQGRvY3MgUGVybWlzc2lvbiwgU2VydmVyLCBTZXJ2ZXJFcnJvciwgaW5pdGlhbGl6ZSwgY3JlYXRlU2VydmVyXG5cbiMjIFJlcXVlc3RzXG5cbkBkb2NzIFJlcXVlc3QsIE1ldGhvZCwgbWV0aG9kVG9TdHJpbmcsIGJvZHlBc1N0cmluZywgYm9keUZyb21Kc29uLCByZXF1ZXN0SW5mb1xuXG4jIyBSZXNwb25kaW5nIHRvIHJlcXVlc3RzXG5cbkBkb2NzIG9uUmVxdWVzdFxuXG5TZWUgW0h0dHBTZXJ2ZXIuUmVzcG9uc2VdKEh0dHBTZXJ2ZXIuUmVzcG9uc2UpIGZvciBtb3JlIGRldGFpbHMgb24gcmVzcG9uZGluZyB0byByZXF1ZXN0cy5cblxuLX1cblxuaW1wb3J0IEJ5dGVzIGV4cG9zaW5nIChCeXRlcylcbmltcG9ydCBCeXRlcy5EZWNvZGUgYXMgRGVjb2RlXG5pbXBvcnQgRGljdCBleHBvc2luZyAoRGljdClcbmltcG9ydCBJbml0XG5pbXBvcnQgSW50ZXJuYWwuSW5pdFxuaW1wb3J0IEpzb24uRW5jb2RlXG5pbXBvcnQgSnNvbi5EZWNvZGVcbmltcG9ydCBOb2RlXG5pbXBvcnQgVGFzayBleHBvc2luZyAoVGFzaylcbmltcG9ydCBHcmVuLktlcm5lbC5IdHRwU2VydmVyXG5pbXBvcnQgSHR0cFNlcnZlci5SZXNwb25zZSBleHBvc2luZyAoUmVzcG9uc2UoLi4pKVxuaW1wb3J0IFVybCBleHBvc2luZyAoVXJsLCBQcm90b2NvbCguLikpXG5cblxuLS0gSU5JVElBTElaQVRJT05cblxuXG57LXwgVGhlIHBlcm1pc3Npb24gdG8gc3RhcnQgYSBbYFNlcnZlcmBdKEh0dHBTZXJ2ZXIuU2VydmVyKS5cblxuWW91IGdldCB0aGlzIGZyb20gW2Bpbml0aWFsaXplYF0oSHR0cFNlcnZlci5pbml0aWFsaXplKS5cbi19XG50eXBlIFBlcm1pc3Npb25cbiAgICA9IFBlcm1pc3Npb25cblxuey18IFRoZSBIVFRQIHNlcnZlci5cbi19XG50eXBlIFNlcnZlclxuICAgIC0tIE5vdGU6IEFjdHVhbCBpbXBsZW1lbnRhdGlvbiBpbiBLZXJuZWwgY29kZVxuICAgID0gU2VydmVyXG5cblxuey18IEVycm9yIGNvZGUgYW5kIG1lc3NhZ2UgZnJvbSBub2RlLlxuTW9zdCBsaWtlbHkgZnJvbSBhIGZhaWxlZCBhdHRlbXB0IHRvIHN0YXJ0IHRoZSBzZXJ2ZXIgKGUuZy4gYEVBRERSSU5VU0VgKS5cblJlZmVyIHRvIHRoZSBbbm9kZSBkb2NzXShodHRwczovL25vZGVqcy5vcmcvZG9jcy9sYXRlc3QtdjE4LngvYXBpL2Vycm9ycy5odG1sKSBmb3IgZGV0YWlscy5cbi19XG50eXBlIFNlcnZlckVycm9yID1cbiAgICBTZXJ2ZXJFcnJvciBTdHJpbmcgU3RyaW5nXG5cblxuey18IEluaXRpYWxpemUgdGhlIFtgSHR0cFNlcnZlcmBdKEh0dHBTZXJ2ZXIpIG1vZHVsZSBhbmQgZ2V0IHBlcm1pc3Npb24gdG8gY3JlYXRlIGEgc2VydmVyLlxuLX1cbmluaXRpYWxpemUgOiBJbml0LlRhc2sgUGVybWlzc2lvblxuaW5pdGlhbGl6ZSA9XG4gICAgVGFzay5zdWNjZWVkIFBlcm1pc3Npb25cbiAgICAgICAgfD4gSW50ZXJuYWwuSW5pdC5UYXNrXG5cblxuXG57LXwgVGFzayB0byBpbml0aWFsaXplIGEgW2BTZXJ2ZXJgXShIdHRwU2VydmVyI1NlcnZlcikuXG4tfVxuY3JlYXRlU2VydmVyIDogUGVybWlzc2lvbiAtPiB7IGhvc3QgOiBTdHJpbmcsIHBvcnRfIDogSW50IH0gLT4gVGFzayBTZXJ2ZXJFcnJvciBTZXJ2ZXJcbmNyZWF0ZVNlcnZlciBfIG9wdGlvbnMgPVxuICAgIEdyZW4uS2VybmVsLkh0dHBTZXJ2ZXIuY3JlYXRlU2VydmVyIG9wdGlvbnMuaG9zdCBvcHRpb25zLnBvcnRfXG5cblxuLS0gUkVRVUVTVFNcblxuXG57LXwgQW4gaW5jb21pbmcgSFRUUCByZXFlc3QuXG4tfVxudHlwZSBhbGlhcyBSZXF1ZXN0ID1cbiAgICB7IGhlYWRlcnMgOiBEaWN0IFN0cmluZyBTdHJpbmdcbiAgICAsIG1ldGhvZCA6IE1ldGhvZFxuICAgICwgYm9keSA6IEJ5dGVzXG4gICAgLCB1cmwgOiBVcmxcbiAgIH1cblxuXG57LXwgSFRUUCByZXF1ZXN0IG1ldGhvZHMuXG4tfVxudHlwZSBNZXRob2RcbiAgICA9IEdFVFxuICAgIHwgSEVBRFxuICAgIHwgUE9TVFxuICAgIHwgUFVUXG4gICAgfCBERUxFVEVcbiAgICB8IENPTk5FQ1RcbiAgICB8IFRSQUNFXG4gICAgfCBQQVRDSFxuICAgIHwgVU5LTk9XTiBTdHJpbmdcblxuXG57LXwgU3RyaW5nIHJlcHJlc2VudGF0aW9uIG9mIG1ldGhvZFxuLX1cbm1ldGhvZFRvU3RyaW5nIDogTWV0aG9kIC0+IFN0cmluZ1xubWV0aG9kVG9TdHJpbmcgbWV0aG9kID1cbiAgICBjYXNlIG1ldGhvZCBvZlxuICAgICAgICBHRVQgLT5cbiAgICAgICAgICAgIFwiR0VUXCJcbiAgICAgICAgXG4gICAgICAgIEhFQUQgLT5cbiAgICAgICAgICAgIFwiSEVBRFwiXG4gICAgICAgIFxuICAgICAgICBQT1NUIC0+XG4gICAgICAgICAgICBcIlBPU1RcIlxuICAgICAgICBcbiAgICAgICAgUFVUIC0+XG4gICAgICAgICAgICBcIlBVVFwiXG4gICAgICAgIFxuICAgICAgICBERUxFVEUgLT5cbiAgICAgICAgICAgIFwiREVMRVRFXCJcbiAgICAgICAgXG4gICAgICAgIENPTk5FQ1QgLT5cbiAgICAgICAgICAgIFwiQ09OTkVDVFwiXG4gICAgICAgIFxuICAgICAgICBUUkFDRSAtPlxuICAgICAgICAgICAgXCJUUkFDRVwiXG4gICAgICAgIFxuICAgICAgICBQQVRDSCAtPlxuICAgICAgICAgICAgXCJQQVRDSFwiXG4gICAgICAgIFxuICAgICAgICBVTktOT1dOIHZhbHVlIC0+XG4gICAgICAgICAgICB2YWx1ZVxuXG5cbnstfCBUdXJuIHRoZSBwaWVjZXMgb2YgYSByZXF1ZXN0IGludG8gYSBbYFJlcXVlc3RgXShIdHRwU2VydmVyLlJlcXVlc3QpIHJlY29yZC5cblxuVGhpcyBpcyBvbmx5IHVzZWQgaW50ZXJuYWxseS5cbi19XG50b1JlcXVlc3QgOlxuICAgIHsgdXJsUHJvdG9jb2wgOiBTdHJpbmdcbiAgICAsIHVybEhvc3QgOiBTdHJpbmdcbiAgICAsIHVybFBvcnQgOiBTdHJpbmdcbiAgICAsIHVybFBhdGggOiBTdHJpbmdcbiAgICAsIHVybFF1ZXJ5IDogU3RyaW5nXG4gICAgLCB1cmxGcmFnbWVudCA6IFN0cmluZ1xuICAgICwgaGVhZGVycyA6IEFycmF5IFN0cmluZ1xuICAgICwgbWV0aG9kIDogU3RyaW5nXG4gICAgLCBib2R5IDogQnl0ZXNcbiAgICB9XG4gICAgLT4gUmVxdWVzdFxudG9SZXF1ZXN0IFxuICAgIHsgdXJsUHJvdG9jb2xcbiAgICAsIHVybEhvc3RcbiAgICAsIHVybFBvcnRcbiAgICAsIHVybFBhdGhcbiAgICAsIHVybFF1ZXJ5XG4gICAgLCB1cmxGcmFnbWVudFxuICAgICwgaGVhZGVyc1xuICAgICwgbWV0aG9kXG4gICAgLCBib2R5IFxuICAgIH0gPVxuICAgIHsgbWV0aG9kID0gdG9NZXRob2QgbWV0aG9kXG4gICAgLCBib2R5ID0gYm9keVxuICAgICwgaGVhZGVycyA9XG4gICAgICAgIGhlYWRlcnNcbiAgICAgICAgICAgIHw+IGFycmF5UGFpcnNcbiAgICAgICAgICAgIHw+IGRpY3RGcm9tUGFpcnNcbiAgICAsIHVybCA9XG4gICAgICAgIHsgcHJvdG9jb2wgPSBcbiAgICAgICAgICAgIGlmIHVybFByb3RvY29sID09IFwiaHR0cHM6XCIgdGhlblxuICAgICAgICAgICAgICAgIEh0dHBzXG4gICAgICAgICAgICBlbHNlXG4gICAgICAgICAgICAgICAgSHR0cFxuICAgICAgICAsIGhvc3QgPSB1cmxIb3N0XG4gICAgICAgICwgcGF0aCA9IHVybFBhdGhcbiAgICAgICAgLCBwb3J0XyA9IFN0cmluZy50b0ludCB1cmxQb3J0XG4gICAgICAgICwgcXVlcnkgPSBcbiAgICAgICAgICAgIGlmIHVybFF1ZXJ5ID09IFwiXCIgdGhlblxuICAgICAgICAgICAgICAgIE5vdGhpbmdcbiAgICAgICAgICAgIGVsc2VcbiAgICAgICAgICAgICAgICBKdXN0IHVybFF1ZXJ5XG4gICAgICAgICwgZnJhZ21lbnQgPVxuICAgICAgICAgICAgaWYgdXJsRnJhZ21lbnQgPT0gXCJcIiB0aGVuXG4gICAgICAgICAgICAgICAgTm90aGluZ1xuICAgICAgICAgICAgZWxzZVxuICAgICAgICAgICAgICAgIEp1c3QgdXJsRnJhZ21lbnRcbiAgICAgICAgfVxuICAgIH1cblxuXG57LXwgR2V0IHJlcXVlc3QgYm9keSBhcyBhIHN0cmluZy5cbi19XG5ib2R5QXNTdHJpbmcgOiBSZXF1ZXN0IC0+IE1heWJlIFN0cmluZ1xuYm9keUFzU3RyaW5nIHJlcSA9XG4gICAgQnl0ZXMudG9TdHJpbmcgcmVxLmJvZHlcblxuXG57LXwgR2V0IHJlcXVlc3QgYm9keSBhcyBqc29uLlxuLX1cbmJvZHlGcm9tSnNvbiA6IEpzb24uRGVjb2RlLkRlY29kZXIgYSAtPiBSZXF1ZXN0IC0+IFJlc3VsdCBKc29uLkRlY29kZS5FcnJvciBhXG5ib2R5RnJvbUpzb24gZGVjb2RlciByZXEgPVxuICAgIHJlcVxuICAgICAgICB8PiBib2R5QXNTdHJpbmdcbiAgICAgICAgfD4gTWF5YmUud2l0aERlZmF1bHQgXCJcIiAtLSBvciBiZXR0ZXIgaWYgcmVzdWx0IGhvbGRzIGEgbWF5YmU/XG4gICAgICAgIHw+IEpzb24uRGVjb2RlLmRlY29kZVN0cmluZyBkZWNvZGVyXG5cblxuey18IEdldCBhIHN0cmluZyByZXByZXNlbnRhdGlvbiBvZiB0aGUgcmVxdWVzdC5cblxuR29vZCBmb3IgbG9nZ2luZy5cbi19XG5yZXF1ZXN0SW5mbyA6IFJlcXVlc3QgLT4gU3RyaW5nXG5yZXF1ZXN0SW5mbyByZXEgPVxuICAgIGxldFxuICAgICAgICBtZXRob2RcbiAgICAgICAgICAgID0gY2FzZSByZXEubWV0aG9kIG9mXG4gICAgICAgICAgICAgICAgR0VUIC0+XG4gICAgICAgICAgICAgICAgICAgIFwiR0VUXCJcblxuICAgICAgICAgICAgICAgIEhFQUQgLT5cbiAgICAgICAgICAgICAgICAgICAgXCJIRUFEXCJcblxuICAgICAgICAgICAgICAgIFBPU1QgLT5cbiAgICAgICAgICAgICAgICAgICAgXCJQT1NUXCJcblxuICAgICAgICAgICAgICAgIFBVVCAtPlxuICAgICAgICAgICAgICAgICAgICBcIlBVVFwiXG5cbiAgICAgICAgICAgICAgICBERUxFVEUgLT5cbiAgICAgICAgICAgICAgICAgICAgXCJERUxFVEVcIlxuXG4gICAgICAgICAgICAgICAgQ09OTkVDVCAtPlxuICAgICAgICAgICAgICAgICAgICBcIkNPTk5FQ1RcIlxuXG4gICAgICAgICAgICAgICAgVFJBQ0UgLT5cbiAgICAgICAgICAgICAgICAgICAgXCJUUkFDRVwiXG5cbiAgICAgICAgICAgICAgICBQQVRDSCAtPlxuICAgICAgICAgICAgICAgICAgICBcIlBBVENIXCJcblxuICAgICAgICAgICAgICAgIFVOS05PV04gbSAtPlxuICAgICAgICAgICAgICAgICAgICBcIlVOS05PV04oXCIgKysgbSArKyBcIilcIlxuICAgIGluXG4gICAgbWV0aG9kICsrIFwiIFwiICsrIHJlcS51cmwucGF0aFxuXG5cbnRvTWV0aG9kIDogU3RyaW5nIC0+IE1ldGhvZFxudG9NZXRob2QgcyA9XG4gICAgY2FzZSBzIG9mXG4gICAgICAgIFwiR0VUXCIgLT5cbiAgICAgICAgICAgIEdFVFxuICAgICAgICBcbiAgICAgICAgXCJIRUFEXCIgLT5cbiAgICAgICAgICAgIEhFQURcblxuICAgICAgICBcIlBPU1RcIiAtPlxuICAgICAgICAgICAgUE9TVFxuICAgICAgICBcbiAgICAgICAgXCJQVVRcIiAtPlxuICAgICAgICAgICAgUFVUXG4gICAgICAgIFxuICAgICAgICBcIkRFTEVURVwiIC0+XG4gICAgICAgICAgICBERUxFVEVcbiAgICAgICAgXG4gICAgICAgIFwiQ09OTkVDVFwiIC0+XG4gICAgICAgICAgICBDT05ORUNUXG4gICAgICAgIFxuICAgICAgICBcIlRSQUNFXCIgLT5cbiAgICAgICAgICAgIFRSQUNFXG4gICAgICAgIFxuICAgICAgICBcIlBBVENIXCIgLT5cbiAgICAgICAgICAgIFBBVENIXG4gICAgICAgIFxuICAgICAgICBfIC0+XG4gICAgICAgICAgICBVTktOT1dOIHNcblxuXG5hcnJheVBhaXJzIDogQXJyYXkgU3RyaW5nIC0+IEFycmF5IChBcnJheSBTdHJpbmcpXG5hcnJheVBhaXJzIGEgPVxuICAgIGxldFxuICAgICAgICBwYWlyID0gXG4gICAgICAgICAgICBBcnJheS50YWtlRmlyc3QgMiBhXG4gICAgICAgIFxuICAgICAgICByZXN0ID1cbiAgICAgICAgICAgIEFycmF5LmRyb3BGaXJzdCAyIGFcbiAgICAgICAgXG4gICAgICAgIGFsbFBhaXJzID1cbiAgICAgICAgICAgIFsgcGFpciBdICsrIGNhc2UgcmVzdCBvZlxuICAgICAgICAgICAgICAgIFtdIC0+XG4gICAgICAgICAgICAgICAgICAgIFtdXG4gICAgICAgICAgICAgICAgXG4gICAgICAgICAgICAgICAgXyAtPlxuICAgICAgICAgICAgICAgICAgICBhcnJheVBhaXJzIHJlc3RcbiAgICBpblxuICAgIGFsbFBhaXJzXG5cblxuZGljdEZyb21QYWlycyA6IEFycmF5IChBcnJheSBTdHJpbmcpIC0+IERpY3QgU3RyaW5nIFN0cmluZ1xuZGljdEZyb21QYWlycyBwYWlycyA9XG4gICAgbGV0XG4gICAgICAgIG1hcHBlciBwIGRpY3QgPVxuICAgICAgICAgICAgY2FzZSBwIG9mXG4gICAgICAgICAgICAgICAgW2EsIGJdIC0+XG4gICAgICAgICAgICAgICAgICAgIERpY3Quc2V0IGEgYiBkaWN0XG4gICAgICAgICAgICAgICAgXG4gICAgICAgICAgICAgICAgXyAtPlxuICAgICAgICAgICAgICAgICAgICBkaWN0XG4gICAgaW5cbiAgICBBcnJheS5mb2xkbCBtYXBwZXIgRGljdC5lbXB0eSBwYWlyc1xuXG5cbi0tIEVGRkVDVCBTVFVGRlxuXG5cbnR5cGUgSHR0cFN1YiBtc2dcbiAgICA9IE9uUmVxdWVzdFN1YiBTZXJ2ZXIgKFJlcXVlc3QgLT4gUmVzcG9uc2UgLT4gbXNnKVxuXG5cbnN1Yk1hcCA6IChhIC0+IGIpIC0+IEh0dHBTdWIgYSAtPiBIdHRwU3ViIGJcbnN1Yk1hcCBmIHN1YiA9XG4gICAgY2FzZSBzdWIgb2ZcbiAgICAgICAgT25SZXF1ZXN0U3ViIHNlcnZlciBtc2cgLT5cbiAgICAgICAgICAgIE9uUmVxdWVzdFN1YiBzZXJ2ZXIgKFxccmVxIHJlcyAtPiBmIChtc2cgcmVxIHJlcykpXG5cblxudHlwZSBhbGlhcyBTdGF0ZSBtc2cgPVxuICAgIEFycmF5IChIdHRwU3ViIG1zZylcblxuXG5pbml0IDogVGFzayBOZXZlciAoU3RhdGUgbXNnKVxuaW5pdCA9XG4gICAgVGFzay5zdWNjZWVkIFtdXG5cblxub25FZmZlY3RzXG4gICAgOiBQbGF0Zm9ybS5Sb3V0ZXIgbXNnIFNlbGZNc2dcbiAgICAtPiBBcnJheSAoSHR0cFN1YiBtc2cpXG4gICAgLT4gU3RhdGUgbXNnXG4gICAgLT4gVGFzayBOZXZlciAoU3RhdGUgbXNnKVxub25FZmZlY3RzIHJvdXRlciBzdWJzIHN0YXRlID1cbiAgICBsZXRcbiAgICAgICAgX3JlbW92ZUxpc3RlbmVycyA9XG4gICAgICAgICAgICBzdGF0ZVxuICAgICAgICAgICAgICAgIHw+IEFycmF5Lm1hcFxuICAgICAgICAgICAgICAgICAgICAoXFwoT25SZXF1ZXN0U3ViIHNlcnZlciBfKSAtPlxuICAgICAgICAgICAgICAgICAgICAgICAgR3Jlbi5LZXJuZWwuSHR0cFNlcnZlci5yZW1vdmVBbGxMaXN0ZW5lcnMgc2VydmVyXG4gICAgICAgICAgICAgICAgICAgIClcbiAgICAgICAgXG4gICAgICAgIF9hZGRMaXN0ZW5lcnMgPVxuICAgICAgICAgICAgc3Vic1xuICAgICAgICAgICAgICAgIHw+IEFycmF5Lm1hcFxuICAgICAgICAgICAgICAgICAgICAoXFwoT25SZXF1ZXN0U3ViIHNlcnZlciBtc2cpIC0+XG4gICAgICAgICAgICAgICAgICAgICAgICBHcmVuLktlcm5lbC5IdHRwU2VydmVyLmFkZExpc3RlbmVyIHNlcnZlciByb3V0ZXIgbXNnXG4gICAgICAgICAgICAgICAgICAgIClcbiAgICBpblxuICAgIFRhc2suc3VjY2VlZCBzdWJzXG5cblxudHlwZSBTZWxmTXNnID1cbiAgICBOZXZlclxuXG5cbm9uU2VsZk1zZyA6IFBsYXRmb3JtLlJvdXRlciBtc2cgU2VsZk1zZyAtPiBTZWxmTXNnIC0+IChTdGF0ZSBtc2cpIC0+IFRhc2sgTmV2ZXIgKFN0YXRlIG1zZylcbm9uU2VsZk1zZyBfIF8gc3RhdGUgPVxuICAgIFRhc2suc3VjY2VlZCBzdGF0ZVxuXG5cbnstfCBTdWJzY3JpYmUgdG8gaW5jb21pbmcgSFRUUCByZXF1ZXN0cy5cbi19XG5vblJlcXVlc3QgOiBTZXJ2ZXIgLT4gKFJlcXVlc3QgLT4gUmVzcG9uc2UgLT4gbXNnKSAtPiBTdWIgbXNnXG5vblJlcXVlc3Qgc2VydmVyIHRhZ2dlciA9XG4gICAgc3Vic2NyaXB0aW9uIChPblJlcXVlc3RTdWIgc2VydmVyIHRhZ2dlcilcbiIsCiAgICAgICAgIm1vZHVsZSBQcm9jZXNzIGV4cG9zaW5nIChJZCwgc3Bhd24sIHNsZWVwLCBraWxsKVxuXG57LXwgQSBwcm9jZXNzIGlzIGVzc2VudGlhbGx5IGEgZnVuY3Rpb24gcnVubmluZyBjb25jdXJyZW50bHkgdG8geW91ciBhcHBsaWNhdGlvbi5cblxuXG5AZG9jcyBJZCwgc3Bhd24sIHNsZWVwLCBraWxsXG5cblxuIyMgRnV0dXJlIFBsYW5zXG5cblJpZ2h0IG5vdywgdGhpcyBsaWJyYXJ5IGlzIHByZXR0eSBzcGFyc2UuIEZvciBleGFtcGxlLCB0aGVyZSBpcyBubyBwdWJsaWMgQVBJXG5mb3IgcHJvY2Vzc2VzIHRvIGNvbW11bmljYXRlIHdpdGggZWFjaCBvdGhlci4gVGhpcyBpcyBhIHJlYWxseSBpbXBvcnRhbnRcbmFiaWxpdHksIGJ1dCBpdCBpcyBhbHNvIHNvbWV0aGluZyB0aGF0IGlzIGV4dHJhb3JkaW5hcmlseSBlYXN5IHRvIGdldCB3cm9uZyFcblxuSSB0aGluayB0aGUgdHJlbmQgd2lsbCBiZSB0b3dhcmRzIGFuIEVybGFuZyBzdHlsZSBvZiBjb25jdXJyZW5jeSwgd2hlcmUgZXZlcnlcbnByb2Nlc3MgaGFzIGFuIOKAnGV2ZW50IHF1ZXVl4oCdIHRoYXQgYW55b25lIGNhbiBzZW5kIG1lc3NhZ2VzIHRvLiBJIGN1cnJlbnRseVxudGhpbmsgdGhlIEFQSSB3aWxsIGJlIGV4dGVuZGVkIHRvIGJlIG1vcmUgbGlrZSB0aGlzOlxuXG4gICAgdHlwZSBJZCBleGl0IG1zZ1xuXG4gICAgc3Bhd24gOiBUYXNrIGV4aXQgYSAtPiBUYXNrIHggKElkIGV4aXQgTmV2ZXIpXG5cbiAgICBraWxsIDogSWQgZXhpdCBtc2cgLT4gVGFzayB4IHt9XG5cbiAgICBzZW5kIDogSWQgZXhpdCBtc2cgLT4gbXNnIC0+IFRhc2sgeCB7fVxuXG5BIHByb2Nlc3MgYElkYCB3aWxsIGhhdmUgdHdvIHR5cGUgdmFyaWFibGVzIHRvIG1ha2Ugc3VyZSBhbGwgY29tbXVuaWNhdGlvbiBpc1xudmFsaWQuIFRoZSBgZXhpdGAgdHlwZSBkZXNjcmliZXMgdGhlIG1lc3NhZ2VzIHRoYXQgYXJlIHByb2R1Y2VkIGlmIHRoZSBwcm9jZXNzXG5mYWlscyBiZWNhdXNlIG9mIHVzZXIgY29kZS4gU28gaWYgcHJvY2Vzc2VzIGFyZSBsaW5rZWQgYW5kIHRyYXBwaW5nIGVycm9ycyxcbnRoZXkgd2lsbCBuZWVkIHRvIGhhbmRsZSB0aGlzLiBUaGUgYG1zZ2AgdHlwZSBqdXN0IGRlc2NyaWJlcyB3aGF0IGtpbmQgb2Zcbm1lc3NhZ2VzIHRoaXMgcHJvY2VzcyBjYW4gYmUgc2VudCBieSBzdHJhbmdlcnMuXG5cbldlIHNoYWxsIHNlZSB0aG91Z2ghIFRoaXMgaXMganVzdCBhIGRyYWZ0IHRoYXQgZG9lcyBub3QgY292ZXIgbmVhcmx5IGV2ZXJ5dGhpbmdcbml0IG5lZWRzIHRvLCBzbyB0aGUgbG9uZy10ZXJtIHZpc2lvbiBmb3IgY29uY3VycmVuY3kgaW4gR3JlbiB3aWxsIGJlIHJvbGxpbmcgb3V0XG5zbG93bHkgYXMgSSBnZXQgbW9yZSBkYXRhIGFuZCBleHBlcmllbmNlLlxuXG5JIGFzayB0aGF0IHBlb3BsZSBidWxsaXNoIG9uIGNvbXBpbGluZyB0byBub2RlLmpzIGtlZXAgdGhpcyBpbiBtaW5kLiBJIHRoaW5rIHdlXG5jYW4gZG8gYmV0dGVyIHRoYW4gdGhlIGhvcGVsZXNzbHkgYmFkIGNvbmN1cnJlbmN5IG1vZGVsIG9mIG5vZGUuanMsIGFuZCBJIGhvcGVcbnRoZSBHcmVuIGNvbW11bml0eSB3aWxsIGJlIHN1cHBvcnRpdmUgb2YgYmVpbmcgbW9yZSBhbWJpdGlvdXMsIGV2ZW4gaWYgaXQgdGFrZXNcbmxvbmdlci4gVGhhdOKAmXMga2luZCBvZiB3aGF0IEdyZW4gaXMgYWxsIGFib3V0LlxuXG4tfVxuXG5pbXBvcnQgQmFzaWNzIGV4cG9zaW5nIChGbG9hdCwgTmV2ZXIpXG5pbXBvcnQgR3Jlbi5LZXJuZWwuUHJvY2Vzc1xuaW1wb3J0IEdyZW4uS2VybmVsLlNjaGVkdWxlclxuaW1wb3J0IFBsYXRmb3JtXG5pbXBvcnQgVGFzayBleHBvc2luZyAoVGFzaylcblxuXG57LXwgQSBsaWdodC13ZWlnaHQgcHJvY2VzcyB0aGF0IHJ1bnMgY29uY3VycmVudGx5LiBZb3UgY2FuIHVzZSBgc3Bhd25gIHRvXG5nZXQgYSBidW5jaCBvZiBkaWZmZXJlbnQgdGFza3MgcnVubmluZyBpbiBkaWZmZXJlbnQgcHJvY2Vzc2VzLiBUaGUgR3JlbiBydW50aW1lXG53aWxsIGludGVybGVhdmUgdGhlaXIgcHJvZ3Jlc3MuIFNvIGlmIGEgdGFzayBpcyB0YWtpbmcgdG9vIGxvbmcsIHdlIHdpbGwgcGF1c2Vcbml0IGF0IGFuIGBhbmRUaGVuYCBhbmQgc3dpdGNoIG92ZXIgdG8gb3RoZXIgc3R1ZmYuXG5cbioqTm90ZToqKiBXZSBtYWtlIGEgZGlzdGluY3Rpb24gYmV0d2VlbiBfY29uY3VycmVuY3lfIHdoaWNoIG1lYW5zIGludGVybGVhdmluZ1xuZGlmZmVyZW50IHNlcXVlbmNlcyBhbmQgX3BhcmFsbGVsaXNtXyB3aGljaCBtZWFucyBydW5uaW5nIGRpZmZlcmVudFxuc2VxdWVuY2VzIGF0IHRoZSBleGFjdCBzYW1lIHRpbWUuIEZvciBleGFtcGxlLCBhXG5bdGltZS1zaGFyaW5nIHN5c3RlbV0oaHR0cHM6Ly9lbi53aWtpcGVkaWEub3JnL3dpa2kvVGltZS1zaGFyaW5nKSBpcyBkZWZpbml0ZWx5XG5jb25jdXJyZW50LCBidXQgbm90IG5lY2Vzc2FyaWx5IHBhcmFsbGVsLiBTbyBldmVuIHRob3VnaCBKUyBydW5zIHdpdGhpbiBhXG5zaW5nbGUgT1MtbGV2ZWwgdGhyZWFkLCBHcmVuIGNhbiBzdGlsbCBydW4gdGhpbmdzIGNvbmN1cnJlbnRseS5cblxuLX1cbnR5cGUgYWxpYXMgSWQgPVxuICAgIFBsYXRmb3JtLlByb2Nlc3NJZFxuXG5cbnstfCBSdW4gYSB0YXNrIGluIGl0cyBvd24gbGlnaHQtd2VpZ2h0IHByb2Nlc3MuIEluIHRoZSBmb2xsb3dpbmcgZXhhbXBsZSxcbmB0YXNrMWAgYW5kIGB0YXNrMmAgd2lsbCBiZSBpbnRlcmxlYXZlZC4gSWYgYHRhc2sxYCBtYWtlcyBhIGxvbmcgSFRUUCByZXF1ZXN0XG5vciBpcyBqdXN0IHRha2luZyBhIGxvbmcgdGltZSwgd2UgY2FuIGhvcCBvdmVyIHRvIGB0YXNrMmAgYW5kIGRvIHNvbWUgd29ya1xudGhlcmUuXG5cbiAgICBzcGF3biB0YXNrMVxuICAgICAgICB8PiBUYXNrLmFuZFRoZW4gKFxcXyAtPiBzcGF3biB0YXNrMilcblxuKipOb3RlOioqIFRoaXMgY3JlYXRlcyBhIHJlbGF0aXZlbHkgcmVzdHJpY3RlZCBraW5kIG9mIGBQcm9jZXNzYCBiZWNhdXNlIGl0XG5jYW5ub3QgcmVjZWl2ZSBhbnkgbWVzc2FnZXMuIE1vcmUgZmxleGliaWxpdHkgZm9yIHVzZXItZGVmaW5lZCBwcm9jZXNzZXMgd2lsbFxuY29tZSBpbiBhIGxhdGVyIHJlbGVhc2UhXG5cbi19XG5zcGF3biA6IFRhc2sgeCBhIC0+IFRhc2sgeSBJZFxuc3Bhd24gPVxuICAgIEdyZW4uS2VybmVsLlNjaGVkdWxlci5zcGF3blxuXG5cbnstfCBCbG9jayBwcm9ncmVzcyBvbiB0aGUgY3VycmVudCBwcm9jZXNzIGZvciB0aGUgZ2l2ZW4gbnVtYmVyIG9mIG1pbGxpc2Vjb25kcy5cblRoZSBKYXZhU2NyaXB0IGVxdWl2YWxlbnQgb2YgdGhpcyBpcyBbYHNldFRpbWVvdXRgXVtzZXRUaW1lb3V0XSB3aGljaCBsZXRzIHlvdVxuZGVsYXkgd29yayB1bnRpbCBsYXRlci5cblxuW3NldFRpbWVvdXRdOiBodHRwczovL2RldmVsb3Blci5tb3ppbGxhLm9yZy9lbi1VUy9kb2NzL1dlYi9BUEkvV2luZG93VGltZXJzL3NldFRpbWVvdXRcblxuLX1cbnNsZWVwIDogRmxvYXQgLT4gVGFzayB4IHt9XG5zbGVlcCA9XG4gICAgR3Jlbi5LZXJuZWwuUHJvY2Vzcy5zbGVlcFxuXG5cbnstfCBTb21ldGltZXMgeW91IGBzcGF3bmAgYSBwcm9jZXNzLCBidXQgbGF0ZXIgZGVjaWRlIGl0IHdvdWxkIGJlIGEgd2FzdGUgdG9cbmhhdmUgaXQga2VlcCBydW5uaW5nIGFuZCBkb2luZyBzdHVmZi4gVGhlIGBraWxsYCBmdW5jdGlvbiB3aWxsIGZvcmNlIGEgcHJvY2Vzc1xudG8gYmFpbCBvbiB3aGF0ZXZlciB0YXNrIGl0IGlzIHJ1bm5pbmcuIFNvIGlmIHRoZXJlIGlzIGFuIEhUVFAgcmVxdWVzdCBpblxuZmxpZ2h0LCBpdCB3aWxsIGFsc28gYWJvcnQgdGhlIHJlcXVlc3QuXG4tfVxua2lsbCA6IElkIC0+IFRhc2sgeCB7fVxua2lsbCA9XG4gICAgR3Jlbi5LZXJuZWwuU2NoZWR1bGVyLmtpbGxcbiIKICAgIF0sCiAgICAibmFtZXMiOiBbCiAgICAgICAgIlRhc2suYW5kVGhlbiIsCiAgICAgICAgIl9TY2hlZHVsZXJfYW5kVGhlbiIsCiAgICAgICAgIkJhc2ljcy5hcEwiLAogICAgICAgICJmIiwKICAgICAgICAieCIsCiAgICAgICAgIkJhc2ljcy5hcFIiLAogICAgICAgICJmdW5jIiwKICAgICAgICAiYWNjIiwKICAgICAgICAiZGljdCIsCiAgICAgICAgImtleSIsCiAgICAgICAgInZhbHVlIiwKICAgICAgICAiRGljdC5mb2xkbCIsCiAgICAgICAgImxlZnQiLAogICAgICAgICJyaWdodCIsCiAgICAgICAgIkFycmF5LnB1c2hMYXN0IiwKICAgICAgICAiX0FycmF5X3B1c2giLAogICAgICAgICJEaWN0LmtleXMiLAogICAgICAgICJrZXlBcnJheSIsCiAgICAgICAgIlNldC50b0FycmF5IiwKICAgICAgICAiX3YwIiwKICAgICAgICAiQmFzaWNzLmFkZCIsCiAgICAgICAgIl9CYXNpY3NfYWRkIiwKICAgICAgICAiU3RyaW5nLmFsbCIsCiAgICAgICAgIl9TdHJpbmdfYWxsIiwKICAgICAgICAiQmFzaWNzLmFuZCIsCiAgICAgICAgIl9CYXNpY3NfYW5kIiwKICAgICAgICAiQmFzaWNzLmFwcGVuZCIsCiAgICAgICAgIl9VdGlsc19hcHBlbmQiLAogICAgICAgICJKc29uLkVuY29kZS5lbmNvZGUiLAogICAgICAgICJfSnNvbl9lbmNvZGUiLAogICAgICAgICJTdHJpbmcuZnJvbUludCIsCiAgICAgICAgIl9TdHJpbmdfZnJvbU51bWJlciIsCiAgICAgICAgIlN0cmluZy5qb2luIiwKICAgICAgICAiX1N0cmluZ19qb2luIiwKICAgICAgICAiU3RyaW5nLnNwbGl0IiwKICAgICAgICAiX1N0cmluZ19zcGxpdCIsCiAgICAgICAgIkpzb24uRGVjb2RlLmluZGVudCIsCiAgICAgICAgInN0ciIsCiAgICAgICAgIkFycmF5LmluZGV4ZWRNYXAiLAogICAgICAgICJfQXJyYXlfaW5kZXhlZE1hcCIsCiAgICAgICAgIkJhc2ljcy5sZSIsCiAgICAgICAgIl9VdGlsc19sZSIsCiAgICAgICAgIkNoYXIudG9Db2RlIiwKICAgICAgICAiX0NoYXJfdG9Db2RlIiwKICAgICAgICAiQ2hhci5pc0xvd2VyIiwKICAgICAgICAiX2NoYXIiLAogICAgICAgICJjb2RlIiwKICAgICAgICAiY2hhciIsCiAgICAgICAgIkNoYXIuaXNVcHBlciIsCiAgICAgICAgIkJhc2ljcy5vciIsCiAgICAgICAgIl9CYXNpY3Nfb3IiLAogICAgICAgICJDaGFyLmlzQWxwaGEiLAogICAgICAgICJDaGFyLmlzRGlnaXQiLAogICAgICAgICJDaGFyLmlzQWxwaGFOdW0iLAogICAgICAgICJBcnJheS5sZW5ndGgiLAogICAgICAgICJfQXJyYXlfbGVuZ3RoIiwKICAgICAgICAiU3RyaW5nLnVuY29ucyIsCiAgICAgICAgIl9TdHJpbmdfdW5jb25zIiwKICAgICAgICAiaSIsCiAgICAgICAgImVycm9yIiwKICAgICAgICAiSnNvbi5EZWNvZGUuZXJyb3JUb1N0cmluZyIsCiAgICAgICAgIkpzb24uRGVjb2RlLmVycm9yVG9TdHJpbmdIZWxwIiwKICAgICAgICAiY29udGV4dCIsCiAgICAgICAgImlzU2ltcGxlIiwKICAgICAgICAiX3YxIiwKICAgICAgICAicmVzdCIsCiAgICAgICAgImZpZWxkTmFtZSIsCiAgICAgICAgImVyciIsCiAgICAgICAgImluZGV4TmFtZSIsCiAgICAgICAgInN0YXJ0ZXIiLAogICAgICAgICJpbnRyb2R1Y3Rpb24iLAogICAgICAgICJlcnJvcnMiLAogICAgICAgICJKc29uLkRlY29kZS5lcnJvck9uZU9mIiwKICAgICAgICAianNvbiIsCiAgICAgICAgIm1zZyIsCiAgICAgICAgIlJlc3VsdC5pc09rIiwKICAgICAgICAicmVzdWx0IiwKICAgICAgICAiRGljdC5lbXB0eSIsCiAgICAgICAgIkRpY3QuUkJFbXB0eV9ncmVuX2J1aWx0aW4iLAogICAgICAgICJEaWN0LmJhbGFuY2UiLAogICAgICAgICJjb2xvciIsCiAgICAgICAgIkRpY3QuUmVkIiwKICAgICAgICAiRGljdC5CbGFjayIsCiAgICAgICAgImxLIiwKICAgICAgICAibFYiLAogICAgICAgICJsTGVmdCIsCiAgICAgICAgImxSaWdodCIsCiAgICAgICAgInJLIiwKICAgICAgICAiclYiLAogICAgICAgICJyTGVmdCIsCiAgICAgICAgInJSaWdodCIsCiAgICAgICAgImxsSyIsCiAgICAgICAgImxsViIsCiAgICAgICAgImxsTGVmdCIsCiAgICAgICAgImxsUmlnaHQiLAogICAgICAgICJCYXNpY3MuY29tcGFyZSIsCiAgICAgICAgIl9VdGlsc19jb21wYXJlIiwKICAgICAgICAibktleSIsCiAgICAgICAgIm5Db2xvciIsCiAgICAgICAgIm5WYWx1ZSIsCiAgICAgICAgIkRpY3Quc2V0SGVscCIsCiAgICAgICAgIm5MZWZ0IiwKICAgICAgICAiblJpZ2h0IiwKICAgICAgICAiRGljdC5zZXQiLAogICAgICAgICJrIiwKICAgICAgICAidiIsCiAgICAgICAgImwiLAogICAgICAgICJyIiwKICAgICAgICAiU3RyaW5nLnRvTG93ZXIiLAogICAgICAgICJfU3RyaW5nX3RvTG93ZXIiLAogICAgICAgICJOb2RlLmFyY2hGcm9tU3RyaW5nIiwKICAgICAgICAiYXJjaCIsCiAgICAgICAgIk5vZGUuQXJtIiwKICAgICAgICAiTm9kZS5Bcm02NCIsCiAgICAgICAgIk5vZGUuSUEzMiIsCiAgICAgICAgIk5vZGUuTWlwcyIsCiAgICAgICAgIk5vZGUuTWlwc2VsIiwKICAgICAgICAiTm9kZS5QUEMiLAogICAgICAgICJOb2RlLlBQQzY0IiwKICAgICAgICAiTm9kZS5TMzkwIiwKICAgICAgICAiTm9kZS5TMzkweCIsCiAgICAgICAgIk5vZGUuWDY0IiwKICAgICAgICAiTm9kZS5Vbmtub3duQXJjaGl0ZWN0dXJlIiwKICAgICAgICAiVGFzay5zdWNjZWVkIiwKICAgICAgICAiX1NjaGVkdWxlcl9zdWNjZWVkIiwKICAgICAgICAiVGFzay5tYXAiLAogICAgICAgICJ0YXNrQSIsCiAgICAgICAgImEiLAogICAgICAgICJOb2RlLnBsYXRmb3JtRnJvbVN0cmluZyIsCiAgICAgICAgInBsYXRmb3JtIiwKICAgICAgICAiTm9kZS5XaW4zMiIsCiAgICAgICAgIk5vZGUuRGFyd2luIiwKICAgICAgICAiTm9kZS5MaW51eCIsCiAgICAgICAgIk5vZGUuRnJlZUJTRCIsCiAgICAgICAgIk5vZGUuT3BlbkJTRCIsCiAgICAgICAgIk5vZGUuU3VuT1MiLAogICAgICAgICJOb2RlLkFpeCIsCiAgICAgICAgIk5vZGUuVW5rbm93blBsYXRmb3JtIiwKICAgICAgICAiTm9kZS5pbml0aWFsaXplRW52aXJvbm1lbnQiLAogICAgICAgICJyYXciLAogICAgICAgICJhcHBsaWNhdGlvblBhdGgiLAogICAgICAgICJhcmdzIiwKICAgICAgICAiY3B1QXJjaGl0ZWN0dXJlIiwKICAgICAgICAic3RkZXJyIiwKICAgICAgICAic3RkaW4iLAogICAgICAgICJzdGRvdXQiLAogICAgICAgICJfTm9kZV9pbml0IiwKICAgICAgICAiVGFzay5pbml0IiwKICAgICAgICAiQXJyYXkubWFwIiwKICAgICAgICAiX0FycmF5X21hcCIsCiAgICAgICAgIkFycmF5LmZvbGRyIiwKICAgICAgICAiX0FycmF5X2ZvbGRyIiwKICAgICAgICAiVGFzay5tYXAyIiwKICAgICAgICAidGFza0IiLAogICAgICAgICJiIiwKICAgICAgICAiQXJyYXkucHJlcGVuZCIsCiAgICAgICAgIl9BcnJheV9hcHBlbmQiLAogICAgICAgICJBcnJheS5wdXNoRmlyc3QiLAogICAgICAgICJhcnJheSIsCiAgICAgICAgIlRhc2suc2VxdWVuY2UiLAogICAgICAgICJ0YXNrcyIsCiAgICAgICAgIlBsYXRmb3JtLnNlbmRUb0FwcCIsCiAgICAgICAgIl9QbGF0Zm9ybV9zZW5kVG9BcHAiLAogICAgICAgICJUYXNrLnNwYXduQ21kIiwKICAgICAgICAicm91dGVyIiwKICAgICAgICAiY21kIiwKICAgICAgICAiX1NjaGVkdWxlcl9zcGF3biIsCiAgICAgICAgInRhc2siLAogICAgICAgICJUYXNrLm9uRWZmZWN0cyIsCiAgICAgICAgImNvbW1hbmRzIiwKICAgICAgICAic3RhdGUiLAogICAgICAgICJUYXNrLm9uU2VsZk1zZyIsCiAgICAgICAgIl92MiIsCiAgICAgICAgIlRhc2suY21kTWFwIiwKICAgICAgICAidGFnZ2VyIiwKICAgICAgICAiVGFzay5QZXJmb3JtIiwKICAgICAgICAiVGFzay5FeGVjdXRlIiwKICAgICAgICAiVGFzay5wZXJmb3JtIiwKICAgICAgICAidG9NZXNzYWdlIiwKICAgICAgICAiVGFzay5jb21tYW5kIiwKICAgICAgICAiTm9kZS51bndyYXAiLAogICAgICAgICJOb2RlLmluaXQiLAogICAgICAgICJpbml0VGFzayIsCiAgICAgICAgImNvbW1hbmQiLAogICAgICAgICJOb2RlLkluaXREb25lIiwKICAgICAgICAiZW52IiwKICAgICAgICAibW9kZWwiLAogICAgICAgICJOb2RlLlVuaW5pdGlhbGl6ZWQiLAogICAgICAgICJQbGF0Zm9ybS5TdWIubWFwIiwKICAgICAgICAiX1BsYXRmb3JtX21hcCIsCiAgICAgICAgIlBsYXRmb3JtLlN1Yi5iYXRjaCIsCiAgICAgICAgIl9QbGF0Zm9ybV9iYXRjaCIsCiAgICAgICAgIlBsYXRmb3JtLlN1Yi5ub25lIiwKICAgICAgICAiTm9kZS5zdWJzY3JpcHRpb25zIiwKICAgICAgICAiYXBwU3VicyIsCiAgICAgICAgIk5vZGUuTXNnUmVjZWl2ZWQiLAogICAgICAgICJhcHBNb2RlbCIsCiAgICAgICAgIlBsYXRmb3JtLkNtZC5tYXAiLAogICAgICAgICJQbGF0Zm9ybS5DbWQuYmF0Y2giLAogICAgICAgICJQbGF0Zm9ybS5DbWQubm9uZSIsCiAgICAgICAgIk5vZGUudXBkYXRlIiwKICAgICAgICAiYXBwVXBkYXRlIiwKICAgICAgICAiaW5pdFJlc3VsdCIsCiAgICAgICAgIk5vZGUuSW5pdGlhbGl6ZWQiLAogICAgICAgICJ1cGRhdGVSZXN1bHQiLAogICAgICAgICJhcHBNc2ciLAogICAgICAgICJQbGF0Zm9ybS53b3JrZXIiLAogICAgICAgICJfUGxhdGZvcm1fd29ya2VyIiwKICAgICAgICAiTm9kZS5kZWZpbmVQcm9ncmFtIiwKICAgICAgICAiY29uZmlnIiwKICAgICAgICAiaW5pdCIsCiAgICAgICAgInN1YnNjcmlwdGlvbnMiLAogICAgICAgICJ1cGRhdGUiLAogICAgICAgICJCYXNpY3MuY29tcG9zZUwiLAogICAgICAgICJnIiwKICAgICAgICAiVGFzay5vbkVycm9yIiwKICAgICAgICAiX1NjaGVkdWxlcl9vbkVycm9yIiwKICAgICAgICAiVGFzay5hdHRlbXB0IiwKICAgICAgICAicmVzdWx0VG9NZXNzYWdlIiwKICAgICAgICAiUmVzdWx0LkVyciIsCiAgICAgICAgIlJlc3VsdC5PayIsCiAgICAgICAgIkJhc2ljcy5pZGVudGl0eSIsCiAgICAgICAgIkluaXQudW53cmFwIiwKICAgICAgICAiSW5pdC5hd2FpdCIsCiAgICAgICAgImZuIiwKICAgICAgICAiSW50ZXJuYWwuSW5pdC5UYXNrIiwKICAgICAgICAiSW5pdC5hd2FpdFRhc2siLAogICAgICAgICJUaW1lLm1pbGxpc1RvUG9zaXgiLAogICAgICAgICJUaW1lLlBvc2l4IiwKICAgICAgICAiRmlsZVN5c3RlbS5jaGVja0FjY2VzcyIsCiAgICAgICAgInBlcm1pc3Npb25zIiwKICAgICAgICAicGF0aCIsCiAgICAgICAgIl9GaWxlU3lzdGVtX2FjY2VzcyIsCiAgICAgICAgIkJhc2ljcy5zdWIiLAogICAgICAgICJfQmFzaWNzX3N1YiIsCiAgICAgICAgIm4iLAogICAgICAgICJyZWMiLAogICAgICAgICJBcnJheS5zbGljZSIsCiAgICAgICAgIl9BcnJheV9zbGljZSIsCiAgICAgICAgIkFycmF5LmRyb3BGaXJzdCIsCiAgICAgICAgIkZpbGVTeXN0ZW0uUGF0aC5lbXB0eSIsCiAgICAgICAgImRpcmVjdG9yeSIsCiAgICAgICAgImV4dGVuc2lvbiIsCiAgICAgICAgImZpbGVuYW1lIiwKICAgICAgICAicm9vdCIsCiAgICAgICAgIkJhc2ljcy5lcSIsCiAgICAgICAgIl9VdGlsc19lcXVhbCIsCiAgICAgICAgIlRhc2suZXhlY3V0ZSIsCiAgICAgICAgIkZpbGVTeXN0ZW0uUGF0aC5mcm9tUG9zaXhTdHJpbmciLAogICAgICAgICJfRmlsZVBhdGhfZnJvbVBvc2l4IiwKICAgICAgICAiRmlsZVN5c3RlbS5QYXRoLmZyb21XaW4zMlN0cmluZyIsCiAgICAgICAgIl9GaWxlUGF0aF9mcm9tV2luMzIiLAogICAgICAgICJ0YXJnZXRLZXkiLAogICAgICAgICJNYXliZS5Ob3RoaW5nIiwKICAgICAgICAiTWF5YmUuSnVzdCIsCiAgICAgICAgIk5vZGUuZ2V0RW52aXJvbm1lbnRWYXJpYWJsZXMiLAogICAgICAgICJfTm9kZV9nZXRFbnZpcm9ubWVudFZhcmlhYmxlcyIsCiAgICAgICAgIkZpbGVTeXN0ZW0uaG9tZURpcmVjdG9yeSIsCiAgICAgICAgIl9GaWxlU3lzdGVtX2hvbWVEaXIiLAogICAgICAgICJDaGlsZFByb2Nlc3MuaW5pdGlhbGl6ZSIsCiAgICAgICAgIkNoaWxkUHJvY2Vzcy5QZXJtaXNzaW9uIiwKICAgICAgICAiRmlsZVN5c3RlbS5pbml0aWFsaXplIiwKICAgICAgICAiRmlsZVN5c3RlbS5QZXJtaXNzaW9uIiwKICAgICAgICAiSHR0cENsaWVudC5pbml0aWFsaXplIiwKICAgICAgICAiSHR0cENsaWVudC5BbnlQZXJtaXNzaW9uIiwKICAgICAgICAiVGVybWluYWwuaW5pdGlhbGl6ZSIsCiAgICAgICAgImlzVFRZIiwKICAgICAgICAiY29sb3JEZXB0aCIsCiAgICAgICAgImNvbHVtbnMiLAogICAgICAgICJwZXJtaXNzaW9uIiwKICAgICAgICAiVGVybWluYWwuUGVybWlzc2lvbiIsCiAgICAgICAgInJvd3MiLAogICAgICAgICJfVGVybWluYWxfaW5pdCIsCiAgICAgICAgIkFycmF5LmFwcGVuZCIsCiAgICAgICAgImZzdCIsCiAgICAgICAgInNlY29uZCIsCiAgICAgICAgIlN0cmluZy5pc0VtcHR5IiwKICAgICAgICAic3RyaW5nIiwKICAgICAgICAiRmlsZVN5c3RlbS5QYXRoLmZpbGVuYW1lV2l0aEV4dGVuc2lvbiIsCiAgICAgICAgIkFycmF5LmZpbHRlciIsCiAgICAgICAgIl9BcnJheV9maWx0ZXIiLAogICAgICAgICJCYXNpY3MubmVxIiwKICAgICAgICAiX1V0aWxzX25vdEVxdWFsIiwKICAgICAgICAiRmlsZVN5c3RlbS5QYXRoLnByZXBlbmQiLAogICAgICAgICJkaXIiLAogICAgICAgICJGaWxlU3lzdGVtLlBhdGguYXBwZW5kIiwKICAgICAgICAiTWFpbi5jb21waWxlclZlcnNpb24iLAogICAgICAgICJNYXliZS5tYXAiLAogICAgICAgICJtYXliZSIsCiAgICAgICAgIk1heWJlLndpdGhEZWZhdWx0IiwKICAgICAgICAiX2RlZmF1bHQiLAogICAgICAgICJkZWZhdWx0IiwKICAgICAgICAiTWFpbi5tYWtlTG9jYWxQYXRoIiwKICAgICAgICAiaG9tZURpciIsCiAgICAgICAgImVudlZhcnMiLAogICAgICAgICJzdGFydFBhdGgiLAogICAgICAgICJEaWN0LmdldCIsCiAgICAgICAgImVuZFBhdGgiLAogICAgICAgICJNYWluLm1ha2VSZW1vdGVQYXRoIiwKICAgICAgICAiQnl0ZXMuRW5jb2RlLmdldExlbmd0aCIsCiAgICAgICAgImJ1aWxkZXIiLAogICAgICAgICJ3IiwKICAgICAgICAiX0J5dGVzX2xlbmd0aCIsCiAgICAgICAgImJzIiwKICAgICAgICAiQXJyYXkuZm9sZGwiLAogICAgICAgICJfQXJyYXlfZm9sZGwiLAogICAgICAgICJtYiIsCiAgICAgICAgIm9mZnNldCIsCiAgICAgICAgIl9CeXRlc193cml0ZV9pOCIsCiAgICAgICAgIl9CeXRlc193cml0ZV9pMTYiLAogICAgICAgICJlIiwKICAgICAgICAiQnl0ZXMuTEUiLAogICAgICAgICJfQnl0ZXNfd3JpdGVfaTMyIiwKICAgICAgICAiX0J5dGVzX3dyaXRlX3U4IiwKICAgICAgICAiX0J5dGVzX3dyaXRlX3UxNiIsCiAgICAgICAgIl9CeXRlc193cml0ZV91MzIiLAogICAgICAgICJfQnl0ZXNfd3JpdGVfZjMyIiwKICAgICAgICAiX0J5dGVzX3dyaXRlX2Y2NCIsCiAgICAgICAgIkJ5dGVzLkVuY29kZS53cml0ZVNlcXVlbmNlIiwKICAgICAgICAiX0J5dGVzX3dyaXRlX3N0cmluZyIsCiAgICAgICAgInMiLAogICAgICAgICJfQnl0ZXNfd3JpdGVfYnl0ZXMiLAogICAgICAgICJidWlsZGVycyIsCiAgICAgICAgImN1cnJlbnRPZmZzZXQiLAogICAgICAgICJCeXRlcy5FbmNvZGUud3JpdGUiLAogICAgICAgICJCeXRlcy5mcm9tU3RyaW5nIiwKICAgICAgICAiX0J5dGVzX2Zyb21TdHJpbmciLAogICAgICAgICJTdHJlYW0uc2VuZCIsCiAgICAgICAgImJ5dGVzIiwKICAgICAgICAiX1N0cmVhbV9zZW5kIiwKICAgICAgICAia2VybmVsU3RyZWFtIiwKICAgICAgICAiU3RyZWFtLnNlbmRTdHJpbmciLAogICAgICAgICJzdHJlYW0iLAogICAgICAgICJTdHJlYW0uc2VuZExpbmUiLAogICAgICAgICJOb2RlLnN0YXJ0UHJvZ3JhbSIsCiAgICAgICAgIkZpbGVTeXN0ZW0uUGF0aC50b1Bvc2l4U3RyaW5nIiwKICAgICAgICAiX0ZpbGVQYXRoX3RvUG9zaXgiLAogICAgICAgICJGaWxlU3lzdGVtLlBhdGgudG9XaW4zMlN0cmluZyIsCiAgICAgICAgIl9GaWxlUGF0aF90b1dpbjMyIiwKICAgICAgICAiTWFpbi5pbml0IiwKICAgICAgICAiZnNQZXJtaXNzaW9uIiwKICAgICAgICAiY3BQZXJtaXNzaW9uIiwKICAgICAgICAiaHR0cFBlcm1pc3Npb24iLAogICAgICAgICJ0ZXJtaW5hbENvbmZpZyIsCiAgICAgICAgInVzZXJBcmdzIiwKICAgICAgICAidXNlbGVzcyIsCiAgICAgICAgIk1haW4uY291bnREb3duIiwKICAgICAgICAidXNlQ29sb3IiLAogICAgICAgICJfdjEwIiwKICAgICAgICAibWF5YmVQYXRocyIsCiAgICAgICAgIm92ZXJyaWRlIiwKICAgICAgICAibG9jYWxQYXRoIiwKICAgICAgICAib3ZlcnJpZGVQYXRoIiwKICAgICAgICAicmVtb3RlUGF0aCIsCiAgICAgICAgInBhdGhzIiwKICAgICAgICAicGF0aFRvU3RyaW5nIiwKICAgICAgICAiTWFpbi5FeGlzdGFuY2VDaGVja2VkIiwKICAgICAgICAiSnNvbi5EZWNvZGUuc3VjY2VlZCIsCiAgICAgICAgIl9Kc29uX3N1Y2NlZWQiLAogICAgICAgICJBcnJheS5maW5kRmlyc3QiLAogICAgICAgICJfQXJyYXlfZmluZEZpcnN0IiwKICAgICAgICAiQXJyYXkubWVtYmVyIiwKICAgICAgICAiRmlsZVN5c3RlbS5hY2Nlc3NQZXJtaXNzaW9uc1RvSW50IiwKICAgICAgICAidmFsdWVzIiwKICAgICAgICAibnVtYmVyRm9yIiwKICAgICAgICAibnVtIiwKICAgICAgICAiRmlsZVN5c3RlbS5SZWFkIiwKICAgICAgICAiRmlsZVN5c3RlbS5Xcml0ZSIsCiAgICAgICAgIkZpbGVTeXN0ZW0uRXhlY3V0ZSIsCiAgICAgICAgIkZpbGVTeXN0ZW0uY2hhbmdlQWNjZXNzIiwKICAgICAgICAibW9kZSIsCiAgICAgICAgIm93bmVyIiwKICAgICAgICAiZ3JvdXAiLAogICAgICAgICJvdGhlcnMiLAogICAgICAgICJfRmlsZVN5c3RlbV9jaG1vZCIsCiAgICAgICAgIkh0dHBDbGllbnQuZXhwZWN0Qnl0ZXMiLAogICAgICAgICJyZXEiLAogICAgICAgICJib2R5IiwKICAgICAgICAiZXhwZWN0IiwKICAgICAgICAiSHR0cENsaWVudC5FeHBlY3RCeXRlcyIsCiAgICAgICAgImhlYWRlcnMiLAogICAgICAgICJtZXRob2QiLAogICAgICAgICJ0aW1lb3V0IiwKICAgICAgICAidXJsIiwKICAgICAgICAiQmFzaWNzLm11bCIsCiAgICAgICAgIl9CYXNpY3NfbXVsIiwKICAgICAgICAiSHR0cENsaWVudC5kZWZhdWx0VGltZW91dCIsCiAgICAgICAgIkh0dHBDbGllbnQucmVxdWVzdCIsCiAgICAgICAgIkh0dHBDbGllbnQuQm9keUVtcHR5IiwKICAgICAgICAiSHR0cENsaWVudC5FeHBlY3RBbnl0aGluZyIsCiAgICAgICAgIkh0dHBDbGllbnQuZ2V0IiwKICAgICAgICAiSHR0cFNlcnZlci5HRVQiLAogICAgICAgICJKc29uLkRlY29kZS5kZWNvZGVTdHJpbmciLAogICAgICAgICJfSnNvbl9ydW5PblN0cmluZyIsCiAgICAgICAgIkh0dHBDbGllbnQuYm9keVR5cGVBc1N0cmluZyIsCiAgICAgICAgIkh0dHBDbGllbnQuZXhwZWN0VHlwZUFzU3RyaW5nIiwKICAgICAgICAiSHR0cFNlcnZlci5tZXRob2RUb1N0cmluZyIsCiAgICAgICAgIkh0dHBDbGllbnQua2VybmVsUmVxdWVzdENvbmZpZyIsCiAgICAgICAgImFjdHVhbFVybCIsCiAgICAgICAgInByZWZpeCIsCiAgICAgICAgImJvZHlUeXBlIiwKICAgICAgICAiZXhwZWN0VHlwZSIsCiAgICAgICAgIkh0dHBDbGllbnQuc2VuZCIsCiAgICAgICAgIl9IdHRwQ2xpZW50X3JlcXVlc3QiLAogICAgICAgICJNYWluLmRvd25sb2FkQmluYXJ5IiwKICAgICAgICAiRmlsZVN5c3RlbS5lcnJvclRvU3RyaW5nIiwKICAgICAgICAibWVzc2FnZSIsCiAgICAgICAgIkh0dHBDbGllbnQuZXJyb3JUb1N0cmluZyIsCiAgICAgICAgInJlcyIsCiAgICAgICAgInN0YXR1c0NvZGUiLAogICAgICAgICJzdGF0dXNUZXh0IiwKICAgICAgICAiZGVidWdTdHIiLAogICAgICAgICJGaWxlU3lzdGVtLm1ha2VEaXJlY3RvcnkiLAogICAgICAgICJvcHRpb25zIiwKICAgICAgICAiX0ZpbGVTeXN0ZW1fbWFrZURpcmVjdG9yeSIsCiAgICAgICAgIkFycmF5LmRyb3BMYXN0IiwKICAgICAgICAiQXJyYXkuZ2V0IiwKICAgICAgICAiX0FycmF5X2dldCIsCiAgICAgICAgIkJhc2ljcy5uZWdhdGUiLAogICAgICAgICJBcnJheS5sYXN0IiwKICAgICAgICAiQXJyYXkucG9wTGFzdCIsCiAgICAgICAgImluaXRpYWwiLAogICAgICAgICJsYXN0IiwKICAgICAgICAiRmlsZVN5c3RlbS5QYXRoLnBhcmVudFBhdGgiLAogICAgICAgICJfdjMiLAogICAgICAgICJleHQiLAogICAgICAgICJmaWxlIiwKICAgICAgICAiQ2hpbGRQcm9jZXNzLmRlZmF1bHRTcGF3bk9wdGlvbnMiLAogICAgICAgICJjb25uZWN0aW9uIiwKICAgICAgICAiQ2hpbGRQcm9jZXNzLkludGVncmF0ZWQiLAogICAgICAgICJlbnZpcm9ubWVudFZhcmlhYmxlcyIsCiAgICAgICAgIkNoaWxkUHJvY2Vzcy5Jbmhlcml0RW52aXJvbm1lbnRWYXJpYWJsZXMiLAogICAgICAgICJydW5EdXJhdGlvbiIsCiAgICAgICAgIkNoaWxkUHJvY2Vzcy5Ob0xpbWl0IiwKICAgICAgICAic2hlbGwiLAogICAgICAgICJDaGlsZFByb2Nlc3MuRGVmYXVsdFNoZWxsIiwKICAgICAgICAid29ya2luZ0RpcmVjdG9yeSIsCiAgICAgICAgIkNoaWxkUHJvY2Vzcy5Jbmhlcml0V29ya2luZ0RpcmVjdG9yeSIsCiAgICAgICAgIkRpY3Quc2luZ2xldG9uIiwKICAgICAgICAiQmFzaWNzLmd0IiwKICAgICAgICAiX1V0aWxzX2d0IiwKICAgICAgICAiQmFzaWNzLm1heCIsCiAgICAgICAgInkiLAogICAgICAgICJQcm9jZXNzLnNwYXduIiwKICAgICAgICAiQ2hpbGRQcm9jZXNzLnNwYXduIiwKICAgICAgICAicHJvZ3JhbSIsCiAgICAgICAgIl9hcmd1bWVudHMiLAogICAgICAgICJvcHRzIiwKICAgICAgICAiX0NoaWxkUHJvY2Vzc19zcGF3biIsCiAgICAgICAgImFyZ3VtZW50cyIsCiAgICAgICAgIm9wdGlvbiIsCiAgICAgICAgIm1zIiwKICAgICAgICAiX3Y0IiwKICAgICAgICAiY2hvaWNlIiwKICAgICAgICAiX3Y1IiwKICAgICAgICAiaW5oZXJpdCIsCiAgICAgICAgIk1haW4ucnVuQ29tcGlsZXIiLAogICAgICAgICJjb2xvckVudlZhciIsCiAgICAgICAgIkNoaWxkUHJvY2Vzcy5NZXJnZVdpdGhFbnZpcm9ubWVudFZhcmlhYmxlcyIsCiAgICAgICAgIkZpbGVTeXN0ZW0ud3JpdGVGaWxlIiwKICAgICAgICAiX0ZpbGVTeXN0ZW1fd3JpdGVGaWxlIiwKICAgICAgICAiTWFpbi51cGRhdGUiLAogICAgICAgICJNYWluLkNvbXBpbGVyRG93bmxvYWRlZCIsCiAgICAgICAgImxvY2F0aW9uIiwKICAgICAgICAiY2FjaGVGb2xkZXIiLAogICAgICAgICJNYWluLkNvbXBpbGVySW5zdGFsbGVkIiwKICAgICAgICAiZGF0YSIsCiAgICAgICAgInJlY3Vyc2l2ZSIsCiAgICAgICAgImZzRXJyIiwKICAgICAgICAiTWFpbi5tYWluIgogICAgXSwKICAgICJtYXBwaW5ncyI6ICI7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7O0lBdVBBQSwrQkFDSUM7SUNnYUpDLHVDQUFJQyxHQUFFQztRQUNGRCxFQUFFQzs7O0lBWk5DLHVDQUFJRCxHQUFFRDtRQUNGQSxFQUFFQzs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7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