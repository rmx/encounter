// Make 'rmx' available in the shell. Everything is in the 'rmx' module
// anyways, and if we need any more symbols we can expose them here.
// global.rmx = rmx;


import { start } from "repl";
start({ prompt: "shard> ", useGlobal: true, ignoreUndefined: true });
