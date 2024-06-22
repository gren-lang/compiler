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
var $gren_lang$core$Basics$apL = F2(function(f, x) {
		return f(x);
	});
var $gren_lang$core$Basics$apR = F2(function(x, f) {
		return f(x);
	});


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
var $gren_lang$core$Json$Decode$Failure = F2(function (a, b) {
		return { $: 'Failure', a: a, b: b };
	});
var $gren_lang$core$Json$Decode$Field = F2(function (a, b) {
		return { $: 'Field', a: a, b: b };
	});
var $gren_lang$core$Json$Decode$Index = F2(function (a, b) {
		return { $: 'Index', a: a, b: b };
	});
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
var $gren_lang$core$Dict$RBNode_gren_builtin = F5(function (a, b, c, d, e) {
		return { $: 'RBNode_gren_builtin', a: a, b: b, c: c, d: d, e: e };
	});
var $gren_lang$core$Dict$Red = { $: 'Red' };
var $gren_lang$core$Dict$balance = F5(function(color, key, value, left, right) {
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
				return A5($gren_lang$core$Dict$RBNode_gren_builtin, $gren_lang$core$Dict$Red, key, value, A5($gren_lang$core$Dict$RBNode_gren_builtin, $gren_lang$core$Dict$Black, lK, lV, lLeft, lRight), A5($gren_lang$core$Dict$RBNode_gren_builtin, $gren_lang$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5($gren_lang$core$Dict$RBNode_gren_builtin, color, rK, rV, A5($gren_lang$core$Dict$RBNode_gren_builtin, $gren_lang$core$Dict$Red, key, value, left, rLeft), rRight);
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
				return A5($gren_lang$core$Dict$RBNode_gren_builtin, $gren_lang$core$Dict$Red, lK, lV, A5($gren_lang$core$Dict$RBNode_gren_builtin, $gren_lang$core$Dict$Black, llK, llV, llLeft, llRight), A5($gren_lang$core$Dict$RBNode_gren_builtin, $gren_lang$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($gren_lang$core$Dict$RBNode_gren_builtin, color, key, value, left, right);
			}
		}
	});
var $gren_lang$core$Basics$compare = _Utils_compare;
var $gren_lang$core$Dict$setHelp = F3(function(key, value, dict) {
		if (dict.$ === 'RBEmpty_gren_builtin') {
			return A5($gren_lang$core$Dict$RBNode_gren_builtin, $gren_lang$core$Dict$Red, key, value, $gren_lang$core$Dict$RBEmpty_gren_builtin, $gren_lang$core$Dict$RBEmpty_gren_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($gren_lang$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5($gren_lang$core$Dict$balance, nColor, nKey, nValue, A3($gren_lang$core$Dict$setHelp, key, value, nLeft), nRight);
				case 'EQ':
					return A5($gren_lang$core$Dict$RBNode_gren_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5($gren_lang$core$Dict$balance, nColor, nKey, nValue, nLeft, A3($gren_lang$core$Dict$setHelp, key, value, nRight));
			}
		}
	});
var $gren_lang$core$Dict$set = F3(function(key, value, dict) {
		var _v0 = A3($gren_lang$core$Dict$setHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_gren_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($gren_lang$core$Dict$RBNode_gren_builtin, $gren_lang$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $gren_lang$node$Internal$Stream$Stream = F2(function (a, b) {
		return { $: 'Stream', a: a, b: b };
	});
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
var $gren_lang$core$Task$map = F2(function(func, taskA) {
		return A2($gren_lang$core$Task$andThen, function(a) {
				return $gren_lang$core$Task$succeed(func(a));
			}, taskA);
	});
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
var $gren_lang$node$Node$initializeEnvironment = A2($gren_lang$core$Task$map, function(raw) {
		return { applicationPath: raw.applicationPath, args: raw.args, cpuArchitecture: $gren_lang$node$Node$archFromString(raw.arch), platform: $gren_lang$node$Node$platformFromString(raw.platform), stderr: A2($gren_lang$node$Internal$Stream$Stream, 1, raw.stderr), stdin: A2($gren_lang$node$Internal$Stream$Stream, 2, raw.stdin), stdout: A2($gren_lang$node$Internal$Stream$Stream, 0, raw.stdout) };
	}, _Node_init);
var $gren_lang$core$Task$Perform = function (a) {
	return { $: 'Perform', a: a };
};
var $gren_lang$core$Task$init = $gren_lang$core$Task$succeed({  });
var $gren_lang$core$Array$map = _Array_map;
var $gren_lang$core$Array$foldr = _Array_foldr;
var $gren_lang$core$Task$map2 = F3(function(func, taskA, taskB) {
		return A2($gren_lang$core$Task$andThen, function(a) {
				return A2($gren_lang$core$Task$andThen, function(b) {
						return $gren_lang$core$Task$succeed(A2(func, a, b));
					}, taskB);
			}, taskA);
	});
var $gren_lang$core$Array$prepend = _Array_append;
var $gren_lang$core$Array$pushFirst = F2(function(value, array) {
		return A2($gren_lang$core$Array$prepend, [ value ], array);
	});
var $gren_lang$core$Task$sequence = function(tasks) {
	return A3($gren_lang$core$Array$foldr, $gren_lang$core$Task$map2($gren_lang$core$Array$pushFirst), $gren_lang$core$Task$succeed([  ]), tasks);
};
var $gren_lang$core$Platform$sendToApp = _Platform_sendToApp;
var $gren_lang$core$Task$spawnCmd = F2(function(router, cmd) {
		if (cmd.$ === 'Perform') {
			var task = cmd.a;
			return _Scheduler_spawn(A2($gren_lang$core$Task$andThen, $gren_lang$core$Platform$sendToApp(router), task));
		} else {
			var task = cmd.a;
			return _Scheduler_spawn(task);
		}
	});
var $gren_lang$core$Task$onEffects = F3(function(router, commands, state) {
		return A2($gren_lang$core$Task$map, function(_v0) {
				return {  };
			}, $gren_lang$core$Task$sequence(A2($gren_lang$core$Array$map, $gren_lang$core$Task$spawnCmd(router), commands)));
	});
var $gren_lang$core$Task$onSelfMsg = F3(function(_v0, _v1, _v2) {
		return $gren_lang$core$Task$succeed({  });
	});
var $gren_lang$core$Task$Execute = function (a) {
	return { $: 'Execute', a: a };
};
var $gren_lang$core$Task$cmdMap = F2(function(tagger, cmd) {
		if (cmd.$ === 'Perform') {
			var task = cmd.a;
			return $gren_lang$core$Task$Perform(A2($gren_lang$core$Task$map, tagger, task));
		} else {
			var task = cmd.a;
			return $gren_lang$core$Task$Execute(task);
		}
	});
_Platform_effectManagers['Task'] = _Platform_createManager($gren_lang$core$Task$init, $gren_lang$core$Task$onEffects, $gren_lang$core$Task$onSelfMsg, $gren_lang$core$Task$cmdMap);
var $gren_lang$core$Task$command = _Platform_leaf('Task');
var $gren_lang$core$Task$perform = F2(function(toMessage, task) {
		return $gren_lang$core$Task$command($gren_lang$core$Task$Perform(A2($gren_lang$core$Task$map, toMessage, task)));
	});
var $gren_lang$node$Node$unwrap = function(_v0) {
	var task = _v0.a;
	return task;
};
var $gren_lang$node$Node$init = F2(function(initTask, _v0) {
		return { command: A2($gren_lang$core$Task$perform, $gren_lang$node$Node$InitDone, A2($gren_lang$core$Task$andThen, function(env) {
					return $gren_lang$node$Node$unwrap(initTask(env));
				}, $gren_lang$node$Node$initializeEnvironment)), model: $gren_lang$node$Node$Uninitialized };
	});
var $gren_lang$node$Node$MsgReceived = function (a) {
	return { $: 'MsgReceived', a: a };
};
var $gren_lang$core$Platform$Sub$map = _Platform_map;
var $gren_lang$core$Platform$Sub$batch = _Platform_batch;
var $gren_lang$core$Platform$Sub$none = $gren_lang$core$Platform$Sub$batch([  ]);
var $gren_lang$node$Node$subscriptions = F2(function(appSubs, model) {
		if (model.$ === 'Uninitialized') {
			return $gren_lang$core$Platform$Sub$none;
		} else {
			var appModel = model.a;
			return A2($gren_lang$core$Platform$Sub$map, $gren_lang$node$Node$MsgReceived, appSubs(appModel));
		}
	});
var $gren_lang$node$Node$Initialized = function (a) {
	return { $: 'Initialized', a: a };
};
var $gren_lang$core$Platform$Cmd$map = _Platform_map;
var $gren_lang$core$Platform$Cmd$batch = _Platform_batch;
var $gren_lang$core$Platform$Cmd$none = $gren_lang$core$Platform$Cmd$batch([  ]);
var $gren_lang$node$Node$update = F3(function(appUpdate, msg, model) {
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
	});
var $gren_lang$core$Platform$worker = _Platform_worker;
var $gren_lang$node$Node$defineProgram = function(config) {
	return $gren_lang$core$Platform$worker({ init: $gren_lang$node$Node$init(config.init), subscriptions: $gren_lang$node$Node$subscriptions(config.subscriptions), update: $gren_lang$node$Node$update(config.update) });
};
var $author$project$Main$ExistanceChecked = function (a) {
	return { $: 'ExistanceChecked', a: a };
};
var $gren_lang$core$Basics$composeL = F3(function(g, f, x) {
		return g(f(x));
	});
var $gren_lang$core$Task$onError = _Scheduler_onError;
var $gren_lang$core$Task$attempt = F2(function(resultToMessage, task) {
		return $gren_lang$core$Task$command($gren_lang$core$Task$Perform(A2($gren_lang$core$Task$onError, A2($gren_lang$core$Basics$composeL, A2($gren_lang$core$Basics$composeL, $gren_lang$core$Task$succeed, resultToMessage), $gren_lang$core$Result$Err), A2($gren_lang$core$Task$andThen, A2($gren_lang$core$Basics$composeL, A2($gren_lang$core$Basics$composeL, $gren_lang$core$Task$succeed, resultToMessage), $gren_lang$core$Result$Ok), task))));
	});
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
var $gren_lang$node$Init$await = F2(function(_v0, fn) {
		var task = _v0.a;
		return $gren_lang$node$Internal$Init$Task(A2($gren_lang$core$Task$andThen, A2($gren_lang$core$Basics$composeL, $gren_lang$node$Init$unwrap, fn), task));
	});
var $gren_lang$node$Init$awaitTask = F2(function(task, fn) {
		return $gren_lang$node$Internal$Init$Task(A2($gren_lang$core$Task$andThen, A2($gren_lang$core$Basics$composeL, $gren_lang$node$Init$unwrap, fn), task));
	});


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
var $gren_lang$node$FileSystem$Error = F2(function (a, b) {
		return { $: 'Error', a: a, b: b };
	});
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
var $gren_lang$node$FileSystem$checkAccess = F3(function(_v0, permissions, path) {
		return A2(_FileSystem_access, permissions, path);
	});
var $gren_lang$core$Array$slice = _Array_slice;
var $gren_lang$core$Array$dropFirst = F2(function(n, array) {
		return A3($gren_lang$core$Array$slice, n, $gren_lang$core$Array$length(array), array);
	});
var $gren_lang$node$FileSystem$Path$empty = { directory: [  ], extension: '', filename: '', root: '' };
var $gren_lang$core$Basics$eq = _Utils_equal;
var $gren_lang$core$Task$execute = function(task) {
	return $gren_lang$core$Task$command($gren_lang$core$Task$Execute(A2($gren_lang$core$Task$map, function(_v0) {
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
var $gren_lang$node$Terminal$initialize = $gren_lang$node$Internal$Init$Task(A2($gren_lang$core$Task$map, function(raw) {
			return raw.isTTY ? $gren_lang$core$Maybe$Just({ colorDepth: raw.colorDepth, columns: raw.columns, permission: $gren_lang$node$Terminal$Permission, rows: raw.rows }) : $gren_lang$core$Maybe$Nothing;
		}, _Terminal_init));
var $gren_lang$core$Array$append = F2(function(fst, second) {
		return A2($gren_lang$core$Array$prepend, second, fst);
	});
var $gren_lang$core$String$isEmpty = function(string) {
	return _Utils_eq(string, '');
};
var $gren_lang$node$FileSystem$Path$filenameWithExtension = function(path) {
	return $gren_lang$core$String$isEmpty(path.extension) ? path.filename : _Utils_ap(path.filename, _Utils_ap('.', path.extension));
};
var $gren_lang$core$Array$filter = _Array_filter;
var $gren_lang$core$Basics$neq = _Utils_notEqual;
var $gren_lang$node$FileSystem$Path$prepend = F2(function(left, right) {
		return _Utils_update(left, { directory: A2($gren_lang$core$Array$filter, function(dir) {
					return !_Utils_eq(dir, '');
				}, A2($gren_lang$core$Array$append, right.directory, A2($gren_lang$core$Array$pushLast, $gren_lang$node$FileSystem$Path$filenameWithExtension(left), left.directory))), extension: right.extension, filename: right.filename });
	});
var $gren_lang$node$FileSystem$Path$append = F2(function(left, right) {
		return A2($gren_lang$node$FileSystem$Path$prepend, right, left);
	});
var $author$project$Main$compilerVersion = '0.4.0';
var $gren_lang$core$Maybe$map = F2(function(f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $gren_lang$core$Maybe$Just(f(value));
		} else {
			return $gren_lang$core$Maybe$Nothing;
		}
	});
var $gren_lang$core$Maybe$withDefault = F2(function(_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Main$makeLocalPath = F3(function(platform, homeDir, envVars) {
		var startPath = function () {
			switch (platform.$) {
				case 'Win32':
					return A2($gren_lang$core$Maybe$withDefault, A2($gren_lang$node$FileSystem$Path$prepend, homeDir, $gren_lang$node$FileSystem$Path$fromPosixString('AppData/Local')), A2($gren_lang$core$Maybe$map, $gren_lang$node$FileSystem$Path$fromWin32String, A2($gren_lang$core$Dict$get, 'LOCALAPPDATA', envVars)));
				case 'Darwin':
					return A2($gren_lang$node$FileSystem$Path$prepend, homeDir, $gren_lang$node$FileSystem$Path$fromPosixString('Library/Caches'));
				default:
					return A2($gren_lang$core$Maybe$withDefault, A2($gren_lang$node$FileSystem$Path$append, $gren_lang$node$FileSystem$Path$fromPosixString('.cache'), homeDir), A2($gren_lang$core$Maybe$map, $gren_lang$node$FileSystem$Path$fromPosixString, A2($gren_lang$core$Dict$get, 'XDG_CACHE_HOME', envVars)));
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
		return A2($gren_lang$node$FileSystem$Path$prepend, startPath, endPath);
	});
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
var $gren_lang$node$Stream$send = F2(function(_v0, bytes) {
		var kernelStream = _v0.b;
		return A2(_Stream_send, kernelStream, bytes);
	});
var $gren_lang$node$Stream$sendString = F2(function(stream, string) {
		return A2($gren_lang$node$Stream$send, stream, $gren_lang$core$Bytes$fromString(string));
	});
var $gren_lang$node$Stream$sendLine = F2(function(stream, string) {
		return A2($gren_lang$node$Stream$sendString, stream, _Utils_ap(string, '\n'));
	});
var $gren_lang$node$Node$startProgram = function(initResult) {
	return $gren_lang$node$Internal$Init$Task($gren_lang$core$Task$succeed(initResult));
};
var $gren_lang$node$FileSystem$Path$toPosixString = _FilePath_toPosix;
var $gren_lang$node$FileSystem$Path$toWin32String = _FilePath_toWin32;
var $author$project$Main$init = function(env) {
	return A2($gren_lang$node$Init$await, $gren_lang$node$FileSystem$initialize, function(fsPermission) {
			return A2($gren_lang$node$Init$await, $gren_lang$node$ChildProcess$initialize, function(cpPermission) {
					return A2($gren_lang$node$Init$await, $gren_lang$node$HttpClient$initialize, function(httpPermission) {
							return A2($gren_lang$node$Init$await, $gren_lang$node$Terminal$initialize, function(terminalConfig) {
									return A2($gren_lang$node$Init$awaitTask, $gren_lang$node$Node$getEnvironmentVariables, function(envVars) {
											return A2($gren_lang$node$Init$awaitTask, $gren_lang$node$FileSystem$homeDirectory(fsPermission), function(homeDir) {
													var userArgs = A2($gren_lang$core$Array$dropFirst, 2, env.args);
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
																				return $gren_lang$core$Maybe$Just({ args: userArgs, localPath: A3($author$project$Main$makeLocalPath, env.platform, homeDir, envVars), remotePath: $gren_lang$core$Maybe$Just($author$project$Main$makeRemotePath('gren.exe')), stdout: env.stdout });
																			} else {
																				break _v2$5;
																			}
																		}
																	case 'Darwin':
																		if (_v2.override.$ === 'Just') {
																			break _v2$1;
																		} else {
																			var _v6 = _v2.platform;
																			return $gren_lang$core$Maybe$Just({ args: userArgs, localPath: A3($author$project$Main$makeLocalPath, env.platform, homeDir, envVars), remotePath: $gren_lang$core$Maybe$Just($author$project$Main$makeRemotePath('gren_mac')), stdout: env.stdout });
																		}
																	case 'Linux':
																		if (_v2.override.$ === 'Just') {
																			break _v2$1;
																		} else {
																			if (_v2.arch.$ === 'X64') {
																				var _v7 = _v2.platform;
																				var _v8 = _v2.arch;
																				return $gren_lang$core$Maybe$Just({ args: userArgs, localPath: A3($author$project$Main$makeLocalPath, env.platform, homeDir, envVars), remotePath: $gren_lang$core$Maybe$Just($author$project$Main$makeRemotePath('gren_linux')), stdout: env.stdout });
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
																return A2($gren_lang$core$Task$attempt, $author$project$Main$ExistanceChecked, A3($gren_lang$node$FileSystem$checkAccess, fsPermission, [  ], model.localPath));
															} else {
																return $gren_lang$core$Task$execute(A2($gren_lang$node$Stream$sendLine, env.stderr, 'We currently don\'t support this platform/arch.'));
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
var $author$project$Main$CompilerExecuted = function (a) {
	return { $: 'CompilerExecuted', a: a };
};
var $author$project$Main$CompilerInstalled = function (a) {
	return { $: 'CompilerInstalled', a: a };
};
var $gren_lang$core$Array$findFirst = _Array_findFirst;
var $gren_lang$core$Array$member = F2(function(value, array) {
		var _v0 = A2($gren_lang$core$Array$findFirst, function(v) {
				return _Utils_eq(v, value);
			}, array);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $gren_lang$node$FileSystem$accessPermissionsToInt = function(values) {
	var numberFor = F2(function(num, a) {
			return A2($gren_lang$core$Array$member, a, values) ? num : 0;
		});
	return (A2(numberFor, 4, $gren_lang$node$FileSystem$Read) + A2(numberFor, 2, $gren_lang$node$FileSystem$Write)) + A2(numberFor, 1, $gren_lang$node$FileSystem$Execute);
};
var $gren_lang$node$FileSystem$changeAccess = F3(function(_v0, permissions, path) {
		var mode = _Utils_ap($gren_lang$core$String$fromInt($gren_lang$node$FileSystem$accessPermissionsToInt(permissions.owner)), _Utils_ap($gren_lang$core$String$fromInt($gren_lang$node$FileSystem$accessPermissionsToInt(permissions.group)), $gren_lang$core$String$fromInt($gren_lang$node$FileSystem$accessPermissionsToInt(permissions.others))));
		return A2(_FileSystem_chmod, mode, path);
	});
var $gren_lang$node$HttpClient$ExpectBytes = { $: 'ExpectBytes' };
var $gren_lang$node$HttpClient$expectBytes = function(req) {
	return { body: req.body, expect: $gren_lang$node$HttpClient$ExpectBytes, headers: req.headers, method: req.method, timeout: req.timeout, url: req.url };
};
var $gren_lang$node$HttpServer$GET = { $: 'GET' };
var $gren_lang$node$HttpClient$BodyEmpty = { $: 'BodyEmpty' };
var $gren_lang$node$HttpClient$ExpectAnything = { $: 'ExpectAnything' };
var $gren_lang$core$Basics$mul = _Basics_mul;
var $gren_lang$node$HttpClient$defaultTimeout = 10 * 1000;
var $gren_lang$node$HttpClient$request = F2(function(method, url) {
		return { body: $gren_lang$node$HttpClient$BodyEmpty, expect: $gren_lang$node$HttpClient$ExpectAnything, headers: $gren_lang$core$Dict$empty, method: method, timeout: $gren_lang$node$HttpClient$defaultTimeout, url: url };
	});
var $gren_lang$node$HttpClient$get = function(url) {
	return A2($gren_lang$node$HttpClient$request, $gren_lang$node$HttpServer$GET, url);
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
var $gren_lang$node$HttpClient$ReceivedChunk = F2(function (a, b) {
		return { $: 'ReceivedChunk', a: a, b: b };
	});
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
var $gren_lang$node$HttpClient$kernelRequestConfig = F2(function(permission, config) {
		var actualUrl = function () {
			if (permission.$ === 'AnyPermission') {
				return config.url;
			} else {
				var prefix = permission.a;
				return _Utils_ap(prefix, config.url);
			}
		}();
		return { body: config.body, bodyType: $gren_lang$node$HttpClient$bodyTypeAsString(config.body), expect: config.expect, expectType: $gren_lang$node$HttpClient$expectTypeAsString(config.expect), headers: config.headers, method: $gren_lang$node$HttpServer$methodToString(config.method), timeout: config.timeout, url: actualUrl };
	});
var $gren_lang$node$HttpClient$send = F2(function(permission, config) {
		return _HttpClient_request(A2($gren_lang$node$HttpClient$kernelRequestConfig, permission, config));
	});
var $author$project$Main$downloadBinary = F2(function(permission, url) {
		return A2($gren_lang$node$HttpClient$send, permission, $gren_lang$node$HttpClient$expectBytes($gren_lang$node$HttpClient$get(url)));
	});
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
var $gren_lang$node$Node$exitWithCode = function(code) {
	return _Node_exitWithCode(code);
};
var $gren_lang$node$FileSystem$makeDirectory = F3(function(_v0, options, path) {
		return A2(_FileSystem_makeDirectory, options, path);
	});
var $gren_lang$core$Basics$sub = _Basics_sub;
var $gren_lang$core$Array$dropLast = F2(function(n, array) {
		return A3($gren_lang$core$Array$slice, 0, $gren_lang$core$Array$length(array) - n, array);
	});
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
		return $gren_lang$core$Maybe$Just({ initial: A2($gren_lang$core$Array$dropLast, 1, array), last: value });
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
var $gren_lang$node$ChildProcess$NoLimit = { $: 'NoLimit' };
var $gren_lang$node$ChildProcess$defaultRunOptions = { environmentVariables: $gren_lang$node$ChildProcess$InheritEnvironmentVariables, maximumBytesWrittenToStreams: 1024 * 1024, runDuration: $gren_lang$node$ChildProcess$NoLimit, shell: $gren_lang$node$ChildProcess$DefaultShell, workingDirectory: $gren_lang$node$ChildProcess$InheritWorkingDirectory };


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
        cwd: workingDir.inherit ? process.cwd() : workingDir.override,
        env:
          env.option === 0
            ? process.env
            : env.option === 1
            ? _Utils_update(process.env, _ChildProcess_dictToObj(env.value))
            : _ChildProcess_dictToObj(env.value),
        timeout: options.runDuration,
        maxBuffer: options.maximumBytesWrittenToStreams,
        shell:
          shell.choice === 0
            ? false
            : shell.choice === 1
            ? true
            : shell.value,
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
var $gren_lang$core$Basics$max = F2(function(x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $gren_lang$node$ChildProcess$run = F4(function(_v0, program, _arguments, opts) {
		return _ChildProcess_run({ _arguments: _arguments, environmentVariables: function () {
				var _v1 = opts.environmentVariables;
				switch (_v1.$) {
					case 'InheritEnvironmentVariables':
						return { option: 0, value: $gren_lang$core$Dict$empty };
					case 'MergeWithEnvironmentVariables':
						var value = _v1.a;
						return { option: 1, value: value };
					default:
						var value = _v1.a;
						return { option: 2, value: value };
				}
			}(), maximumBytesWrittenToStreams: opts.maximumBytesWrittenToStreams, program: program, runDuration: function () {
				var _v2 = opts.runDuration;
				if (_v2.$ === 'NoLimit') {
					return 0;
				} else {
					var ms = _v2.a;
					return A2($gren_lang$core$Basics$max, 0, ms);
				}
			}(), shell: function () {
				var _v3 = opts.shell;
				switch (_v3.$) {
					case 'NoShell':
						return { choice: 0, value: '' };
					case 'DefaultShell':
						return { choice: 1, value: '' };
					default:
						var value = _v3.a;
						return { choice: 2, value: value };
				}
			}(), workingDirectory: function () {
				var _v4 = opts.workingDirectory;
				if (_v4.$ === 'InheritWorkingDirectory') {
					return { inherit: true, override: '' };
				} else {
					var value = _v4.a;
					return { inherit: false, override: value };
				}
			}() });
	});
var $gren_lang$core$Dict$singleton = F2(function(key, value) {
		return A5($gren_lang$core$Dict$RBNode_gren_builtin, $gren_lang$core$Dict$Black, key, value, $gren_lang$core$Dict$RBEmpty_gren_builtin, $gren_lang$core$Dict$RBEmpty_gren_builtin);
	});
var $author$project$Main$runCompiler = function(model) {
	var colorEnvVar = model.useColor ? A2($gren_lang$core$Dict$singleton, 'FORCE_COLOR', '1') : A2($gren_lang$core$Dict$singleton, 'NO_COLOR', '1');
	return A4($gren_lang$node$ChildProcess$run, model.cpPermission, model.pathToString(model.localPath), model.args, _Utils_update($gren_lang$node$ChildProcess$defaultRunOptions, { environmentVariables: $gren_lang$node$ChildProcess$MergeWithEnvironmentVariables(colorEnvVar), maximumBytesWrittenToStreams: 1024 * 1024 }));
};
var $gren_lang$node$FileSystem$writeFile = F3(function(_v0, bytes, path) {
		return A2(_FileSystem_writeFile, bytes, path);
	});
var $author$project$Main$update = F2(function(msg, model) {
		switch (msg.$) {
			case 'ExistanceChecked':
				if (msg.a.$ === 'Err') {
					return { command: function () {
						var _v1 = model.remotePath;
						if (_v1.$ === 'Just') {
							var remotePath = _v1.a;
							return A2($gren_lang$core$Task$attempt, $author$project$Main$CompilerDownloaded, A2($gren_lang$core$Task$andThen, function(_v2) {
										return A2($author$project$Main$downloadBinary, model.httpPermission, remotePath);
									}, A2($gren_lang$node$Stream$sendLine, model.stdout, _Utils_ap('Compiler not found at ', _Utils_ap(model.pathToString(model.localPath), '. Downloading...')))));
						} else {
							return $gren_lang$core$Task$execute(A2($gren_lang$node$Stream$sendLine, model.stderr, _Utils_ap('Compiler not found at ', model.pathToString(model.localPath))));
						}
					}(), model: model };
				} else {
					return { command: A2($gren_lang$core$Task$attempt, $author$project$Main$CompilerExecuted, $author$project$Main$runCompiler(model)), model: model };
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
								return { command: A2($gren_lang$core$Task$attempt, $author$project$Main$CompilerDownloaded, A2($author$project$Main$downloadBinary, model.httpPermission, location)), model: model };
							} else {
								return { command: $gren_lang$core$Task$execute(A2($gren_lang$node$Stream$sendLine, model.stderr, 'Missing, or vague, \'location\' header in 302 response from server.')), model: model };
							}
						} else {
							return { command: $gren_lang$core$Task$execute(A2($gren_lang$node$Stream$sendLine, model.stderr, $gren_lang$node$HttpClient$errorToString(err))), model: model };
						}
					} else {
						var err = msg.a.a;
						return { command: $gren_lang$core$Task$execute(A2($gren_lang$node$Stream$sendLine, model.stderr, $gren_lang$node$HttpClient$errorToString(err))), model: model };
					}
				} else {
					var res = msg.a.a;
					var cacheFolder = A2($gren_lang$core$Maybe$withDefault, $gren_lang$node$FileSystem$Path$empty, $gren_lang$node$FileSystem$Path$parentPath(model.localPath));
					return { command: A2($gren_lang$core$Task$attempt, $author$project$Main$CompilerInstalled, A2($gren_lang$core$Task$andThen, function(_v5) {
								return A2($gren_lang$node$Stream$sendLine, model.stdout, 'Downloaded');
							}, A2($gren_lang$core$Task$andThen, A2($gren_lang$node$FileSystem$changeAccess, model.fsPermission, { group: [ $gren_lang$node$FileSystem$Read, $gren_lang$node$FileSystem$Execute ], others: [ $gren_lang$node$FileSystem$Read, $gren_lang$node$FileSystem$Execute ], owner: [ $gren_lang$node$FileSystem$Read, $gren_lang$node$FileSystem$Write, $gren_lang$node$FileSystem$Execute ] }), A2($gren_lang$core$Task$andThen, function(_v4) {
										return A3($gren_lang$node$FileSystem$writeFile, model.fsPermission, res.data, model.localPath);
									}, A3($gren_lang$node$FileSystem$makeDirectory, model.fsPermission, { recursive: true }, cacheFolder))))), model: model };
				}
			case 'CompilerInstalled':
				if (msg.a.$ === 'Err') {
					var fsErr = msg.a.a;
					return { command: $gren_lang$core$Task$execute(A2($gren_lang$node$Stream$sendLine, model.stderr, _Utils_ap('Failed to install binary after download, due to error: ', $gren_lang$node$FileSystem$errorToString(fsErr)))), model: model };
				} else {
					return { command: A2($gren_lang$core$Task$attempt, $author$project$Main$CompilerExecuted, $author$project$Main$runCompiler(model)), model: model };
				}
			default:
				if (msg.a.$ === 'Err') {
					var output = msg.a.a;
					return { command: $gren_lang$core$Task$execute(A2($gren_lang$core$Task$andThen, function(_v6) {
								return $gren_lang$node$Node$exitWithCode(output.exitCode);
							}, A2($gren_lang$node$Stream$send, model.stderr, output.stderr))), model: model };
				} else {
					var output = msg.a.a;
					return { command: $gren_lang$core$Task$execute(A2($gren_lang$core$Task$andThen, function(_v7) {
								return A2($gren_lang$node$Stream$send, model.stderr, output.stderr);
							}, A2($gren_lang$node$Stream$send, model.stdout, output.stdout))), model: model };
				}
		}
	});
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
