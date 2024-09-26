#!/usr/bin/env node

const compiler = require("./compiler.js");
const compilerInstance = compiler.Gren.Main.init({});

compilerInstance.ports.completeStaticBuild.subscribe(async function (output) {
  const isMac = process.platform === "darwin";
  const isWin = process.platform === "win32";

  const jsBuildPath = output;
  const blobPath = output + ".blob";

  const seaConfigPath = output + ".sea.config";
  const seaConfig = {
    main: jsBuildPath,
    output: blobPath,
    disableExperimentalSEAWarning: true,
    useSnapshot: true,
  };

  const binPath = isWin ? output + ".node.exe" : output + ".node";

  const fs = require("fs/promises");
  const cp = require("child_process");
  const postject = require("postject");

  // For snapshots to work we need to wrap the function call that starts
  // the Gren application, with a hint that tells the V8 engine what the
  // main function is

  const compiledSrc = await fs.readFile(jsBuildPath, "utf-8");

  const initRegex = /this\.Gren\..+\(\{\}\);/g;
  const initCall = compiledSrc.match(initRegex)[0];
  const snapshotCompatibleSrc = compiledSrc.replace(
    initCall,
    `
const v8 = require('node:v8');
v8.startupSnapshot.setDeserializeMainFunction(function() {
  ${initCall}
});
`,
  );

  await fs.writeFile(jsBuildPath, snapshotCompatibleSrc);

  // We then need to generate the snapshot

  const nodePath = process.execPath;
  await fs.writeFile(seaConfigPath, JSON.stringify(seaConfig));
  cp.execFileSync(nodePath, ["--experimental-sea-config", seaConfigPath]);

  // Then copy the node executable and inject the snapshot into it
  await fs.copyFile(nodePath, binPath);

  if (isMac) {
    // required on mac, optional on windows, not required on linux
    cp.execFileSync("codesign", ["--remove-signature", binPath]);
  }

  const blobContent = await fs.readFile(blobPath);

  await postject.inject(binPath, "NODE_SEA_BLOB", blobContent, {
    sentinelFuse: "NODE_SEA_FUSE_fce680ab2cc467b6e072b8b5df1996b2",
    machoSegmentName: isMac ? "NODE_SEA" : undefined,
  });

  if (isMac) {
    // required on mac
    cp.execFileSync("codesign", ["--sign", "-", binPath]);
  }

  // cleanup

  await fs.rm(jsBuildPath);
  await fs.rm(blobPath);
  await fs.rm(seaConfigPath);

  const outputPath = isWin ? jsBuildPath + ".exe" : jsBuildPath;
  await fs.rename(binPath, outputPath);
});
