// This file gives you programmatic access to the Gren compiler from JavaScript.
// TODO: currently copied from node-compiler-library, will need some changes.

import * as fs from "fs/promises";
import * as url from "node:url";
import * as util from "util";

const execFile = util.promisify(childProcess.execFile);

const compilerPath = url.fileURLToPath(await import.meta.resolve("gren-lang"));

/* The version of the Gren compiler that will be downloaded and used for the commands in this package */
export const compilerVersion = "0.4.5";

/* Execute an arbitrary command on the Gren compiler.
 *
 * `path` should be set to the project directory where you wish to execute this command.
 * `args` is an array of arguments passed to the gren compiler.
 * `options` allow you to set environment variables and a timeout (milliseconds).
 */
export async function execute(path, args, options) {
  return await execFile(process.argv[0], [compilerPath].concat(args), {
    cwd: path,
    env: options.env || {},
    timeout: options.timeout || 30_000,
    shell: true,
  });
}

/* Install the dependencies of a Gren project.
 *
 * This executes `gren package install`
 *
 * `path` should be set to the project directory where you wish to execute this command.
 * `options` allow you to set environment variables and a timeout (milliseconds).
 */
export async function installDependencies(path, options) {
  await execute(path, ["package", "install"], options || {});
  return true;
}

/* Compile a Gren project.
 *
 * This executes `gren make` and returns the compiled output, or throws an exception.
 * If you're compiling an application, pass the relative path of the entrypoint as the `target` in the options object.
 *
 * `path` should be set to the project directory where you wish to execute this command.
 * `options` allow you to set environment variables and a timeout. See `execute` for more information.
 *
 * If `options` contains a sourcemaps property that is true, sourcemaps will be generated and inlined into the output.
 */
export async function compileProject(path, options) {
  let args = ["make", "--output=/dev/stdout", "--report=json"];

  if (options.sourcemaps) {
    args.push("--sourcemaps");
  }

  if (options.target) {
    args.push(options.target);
  }

  return handleFailableExecution(path, args, options);
}

async function handleFailableExecution(path, args, options) {
  try {
    const res = await execute(path, args, options);
    return res.stdout;
  } catch (e) {
    let errorData;
    try {
      errorData = JSON.parse(e.stderr);
    } catch (parseErr) {
      // Didn't get error from compiler
      throw e;
    }

    const compileError = new Error(`Failed to compile project: ${path}`);
    for (let key in errorData) {
      compileError[key] = errorData[key];
    }

    throw compileError;
  }
}

/* Compile the documentation of a Gren project.
 *
 * This executes `gren docs` and returns the documentation object, or throws an exception.
 *
 * `path` should be set to the project directory where you wish to execute this command.
 * `options` allow you to set environment variables and a timeout (milliseconds).
 */
export async function compileDocs(path, options) {
  let args = ["docs", "--output=/dev/stdout", "--report=json"];
  const docs = await handleFailableExecution(path, args, options || {});

  return JSON.parse(docs);
}

/* Checks that a Gren project compiles, and (for packages) that the documentation builds.
 *
 * This executes `gren make` or `gren docs` (for packages), and returns true if the project compiles successfully.
 * If you're compiling an application, pass the relative path of the entrypoint as the `target` in the options object.
 *
 * `path` should be set to the project directory where you wish to execute this command.
 * `options` allow you to set environment variables and a timeout (milliseconds).
 */
export async function validateProject(path, opts) {
  let options = opts || {};
  let args;

  if (options.target) {
    args = ["make", "--output=/dev/null", "--report=json", options.target];
  } else {
    args = ["docs", "--output=/dev/null", "--report=json"];
  }

  await handleFailableExecution(path, args, options);

  return true;
}
