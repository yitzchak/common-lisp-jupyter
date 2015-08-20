// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

// adapted by Robert Dodier from Mathematica mode for CodeMirror
// Mathematica mode copyright (c) 2015 by Calin Barbat
// Based on code by Patrick Scheibe (halirutan)
// See: https://github.com/halirutan/Mathematica-Source-Highlighting/tree/master/src/lang-mma.js

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

console.log ('HEY HEY WITHIN MAXIMA.JS #1 ...');

CodeMirror.defineMode('maxima', function(_config, _parserConfig) {

  console.log ('HEY HEY WITHIN MAXIMA.JS #2 ...');

  // used pattern building blocks
  var pIdentifier    = '(?:[a-zA-Z][a-zA-Z0-9]*)';
  var pInteger       = "(?:\\d+|\\d+\\.|0\\[A-Za-z0-9]*)";
  // overlap here with integers ... oh well, not too important to make them mutually exclusive
  var pFloatSansExpt = "(?:\\d+|\\.\\d+|\\d+\\.\\d+)";
  var pFloatExpt     = "(?:[DdEeSsLlFfBb][+-]*\\d+)";

  // regular expressions
  var reIdentifier   = new RegExp (pIdentifier);
  var reInteger      = new RegExp (pInteger);
  var reFloat        = new RegExp ('(?:' + pFloatSansExpt + '|' + pFloatSansExpt + pFloatExpt + ')');

  function tokenBase(stream, state) {
    console.log ('HEY HEY HEY WITHIN MAXIMA.JS #3 ...');

    var ch;

    // get next character
    ch = stream.next();

    // string
    if (ch === '"') {
      state.tokenize = tokenString;
      return state.tokenize(stream, state);
    }

    // comment
    if (ch === '/') {
      if (stream.eat('*')) {
        state.commentLevel++;
        state.tokenize = tokenComment;
        return state.tokenize(stream, state);
      }
    }

    // go back one character
    stream.backUp(1);

    // look for numbers
    if (stream.match(reInteger, true, false)) {
      return 'number';
    }

    if (stream.match(reFloat, true, false)) {
      return 'number';
    }

    // Match all braces separately
    if (stream.match(/(?:\[|\]|{|}|\(|\))/, true, false)) {
      return 'bracket';
    }

    // Literals like variables, keywords, functions
    if (stream.match(reIdentifier, true, false)) {
      return 'keyword';
    }

    // operators. Note that operators like @@ or /; are matched separately for each symbol.
    if (stream.match(/(?:\\|\+|\-|\*|\/|,|;|\.|:|@|~|=|>|<|&|\||_|`|'|\^|\?|!|%)/, true, false)) {
      return 'operator';
    }

    // everything else is an error
    return 'error';
  }

  function tokenString(stream, state) {
    console.log ('HEY HEY HEY WITHIN MAXIMA.JS #4 ...');

    var next, end = false, escaped = false;
    while ((next = stream.next()) != null) {
      if (next === '"' && !escaped) {
        end = true;
        break;
      }
      escaped = !escaped && next === '\\';
    }
    if (end && !escaped) {
      state.tokenize = tokenBase;
    }
    return 'string';
  };

  function tokenComment(stream, state) {
    console.log ('HEY HEY HEY WITHIN MAXIMA.JS #5 ...');

    var prev, next;
    while(state.commentLevel > 0 && (next = stream.next()) != null) {
      if (prev === '/' && next === '*') state.commentLevel++;
      if (prev === '*' && next === '/') state.commentLevel--;
      prev = next;
    }
    if (state.commentLevel <= 0) {
      state.tokenize = tokenBase;
    }
    return 'comment';
  }

  return {
    startState: function() {return {tokenize: tokenBase, commentLevel: 0};},
    token: function(stream, state) {
      if (stream.eatSpace()) return null;
      return state.tokenize(stream, state);
    },
    blockCommentStart: "/*",
    blockCommentEnd: "*/"
  };
});

CodeMirror.defineMIME('text/x-maxima', {
  name: 'maxima'
});

});
