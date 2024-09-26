#!/usr/bin/env node

const compiler = require("./compiler.js");
const compilerInstance = compiler.Gren.Main.init({});

compilerInstance.ports.completeStaticBuild.subscribe(async function (output) {
  const nodePath = process.execPath;
  const jsBuildPath = output;
  const blobPath = output + ".blob";

  const seaConfigPath = output + ".sea.config";
  const seaConfig = {
    main: jsBuildPath,
    output: blobPath,
    disableExperimentalSEAWarning: true,
    useSnapshot: true,
  };

  const binPath = output + ".node";

  const fs = require("fs");
  const cp = require("child_process");
  const postject = require("postject");

  const initRegex = /this\.Gren\..+\(\{\}\);/g;
  const compiledSrc = fs.readFileSync(jsBuildPath, "utf-8");
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
  fs.writeFileSync(jsBuildPath, snapshotCompatibleSrc);

  fs.writeFileSync(seaConfigPath, JSON.stringify(seaConfig));
  cp.execFileSync(nodePath, ["--experimental-sea-config", seaConfigPath]);
  fs.copyFileSync(nodePath, binPath);

  // OS specific
  cp.execFileSync("codesign", ["--remove-signature", binPath]);

  const blobContent = fs.readFileSync(blobPath);

  await postject.inject(binPath, "NODE_SEA_BLOB", blobContent, {
    machoSegmentName: "NODE_SEA",
    sentinelFuse: "NODE_SEA_FUSE_fce680ab2cc467b6e072b8b5df1996b2",
  });

  cp.execFileSync("codesign", ["--sign", "-", binPath]);

  // cleanup

  fs.rmSync(jsBuildPath);
  fs.rmSync(blobPath);
  fs.rmSync(seaConfigPath);
  fs.renameSync(binPath, jsBuildPath);
});
