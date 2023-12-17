/* Small example driver program for compiling/running programs */
const fs = require("fs");
const { exec } = require('child_process');
var args = process.argv.slice(2);

/* We execute this with the path of the file we want to compile */
const compile_cmd = "sh compile.sh"


/* Memory that will be imported in WASM runtime */
const memory = new WebAssembly.Memory({ initial: 1 });


/* Utilities from parsing strings out of C0's memory */
const c0_parse_str = function(address) {
  const bytes = new Uint8Array(memory.buffer.slice(address | 0));
  var i = 0;
  var msg = "";
  while(i < bytes.length && bytes[i] !== undefined && bytes[i] !== 0) {
    msg += String.fromCharCode(bytes[i])
    i += 1;
  }
  return msg;
}

const log_c0_error = function(str) {
  var msg = c0_parse_str(str);
  msg = "error:  " + msg;
  console.log(msg);
}


/* Required imports */
const print_imports = {
  c0deine: {
    memory: memory,
    result: res => { console.log((res | 0)) },
    abort:  sig => { console.log("abort: " + (sig | 0)) },
    error:  log_c0_error,
  }
};


/* Drivers to compile and evaluate programs */
const compile = function(filename, exe, next) {
  exec(compile_cmd + " " + filename,
    (error, stdout, stderr) => {
      if(error !== null) {
        console.log(stdout);
        console.log(stderr);
        return next();
      }
      // console.log(stdout);
      return exe();
    }
  );
}

const run = function(filename, imports) {
  const bytes = fs.readFileSync(filename + ".wasm");
  const wasm = new WebAssembly.Module(bytes);

  try {
    const instance = new WebAssembly.Instance(wasm, imports);
  } catch(e) {
    console.log(e + "");
  }
}


if(!fs.existsSync(args[0])) {
  console.log("Couldn't find file: " + args[0] + "\n");
} else if(fs.lstatSync(args[0]).isFile()) {
  const filename = args[0];
  compile(filename,
    () => { run(filename, print_imports); },
    () => { console.log("Compilation failed."); }
  );
}
