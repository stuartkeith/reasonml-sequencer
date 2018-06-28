// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Random = require("bs-platform/lib/js/random.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var Caml_primitive = require("bs-platform/lib/js/caml_primitive.js");
var Row$ReactTemplate = require("./Row.bs.js");
var Lane$ReactTemplate = require("./Lane.bs.js");
var WebAudio$ReactTemplate = require("./WebAudio.bs.js");

var component = ReasonReact.reducerComponent("App");

function applyToAllLanes(state, fn) {
  return /* Update */Block.__(0, [/* record */[
              /* octave */Curry._1(fn, state[/* octave */0]),
              /* transpose */Curry._1(fn, state[/* transpose */1]),
              /* velocity */Curry._1(fn, state[/* velocity */2]),
              /* chance */Curry._1(fn, state[/* chance */3]),
              /* offset */Curry._1(fn, state[/* offset */4]),
              /* length */Curry._1(fn, state[/* length */5]),
              /* isPlaying */state[/* isPlaying */6],
              /* scheduler */state[/* scheduler */7],
              /* soundBuffer */state[/* soundBuffer */8]
            ]]);
}

function applyToLane(state, laneValue, fn) {
  var tmp;
  switch (laneValue) {
    case 0 : 
        tmp = /* record */[
          /* octave */Curry._1(fn, state[/* octave */0]),
          /* transpose */state[/* transpose */1],
          /* velocity */state[/* velocity */2],
          /* chance */state[/* chance */3],
          /* offset */state[/* offset */4],
          /* length */state[/* length */5],
          /* isPlaying */state[/* isPlaying */6],
          /* scheduler */state[/* scheduler */7],
          /* soundBuffer */state[/* soundBuffer */8]
        ];
        break;
    case 1 : 
        tmp = /* record */[
          /* octave */state[/* octave */0],
          /* transpose */Curry._1(fn, state[/* transpose */1]),
          /* velocity */state[/* velocity */2],
          /* chance */state[/* chance */3],
          /* offset */state[/* offset */4],
          /* length */state[/* length */5],
          /* isPlaying */state[/* isPlaying */6],
          /* scheduler */state[/* scheduler */7],
          /* soundBuffer */state[/* soundBuffer */8]
        ];
        break;
    case 2 : 
        tmp = /* record */[
          /* octave */state[/* octave */0],
          /* transpose */state[/* transpose */1],
          /* velocity */Curry._1(fn, state[/* velocity */2]),
          /* chance */state[/* chance */3],
          /* offset */state[/* offset */4],
          /* length */state[/* length */5],
          /* isPlaying */state[/* isPlaying */6],
          /* scheduler */state[/* scheduler */7],
          /* soundBuffer */state[/* soundBuffer */8]
        ];
        break;
    case 3 : 
        tmp = /* record */[
          /* octave */state[/* octave */0],
          /* transpose */state[/* transpose */1],
          /* velocity */state[/* velocity */2],
          /* chance */Curry._1(fn, state[/* chance */3]),
          /* offset */state[/* offset */4],
          /* length */state[/* length */5],
          /* isPlaying */state[/* isPlaying */6],
          /* scheduler */state[/* scheduler */7],
          /* soundBuffer */state[/* soundBuffer */8]
        ];
        break;
    case 4 : 
        tmp = /* record */[
          /* octave */state[/* octave */0],
          /* transpose */state[/* transpose */1],
          /* velocity */state[/* velocity */2],
          /* chance */state[/* chance */3],
          /* offset */Curry._1(fn, state[/* offset */4]),
          /* length */state[/* length */5],
          /* isPlaying */state[/* isPlaying */6],
          /* scheduler */state[/* scheduler */7],
          /* soundBuffer */state[/* soundBuffer */8]
        ];
        break;
    case 5 : 
        tmp = /* record */[
          /* octave */state[/* octave */0],
          /* transpose */state[/* transpose */1],
          /* velocity */state[/* velocity */2],
          /* chance */state[/* chance */3],
          /* offset */state[/* offset */4],
          /* length */Curry._1(fn, state[/* length */5]),
          /* isPlaying */state[/* isPlaying */6],
          /* scheduler */state[/* scheduler */7],
          /* soundBuffer */state[/* soundBuffer */8]
        ];
        break;
    
  }
  return /* Update */Block.__(0, [tmp]);
}

function onSetValue(send, laneValue, index, value) {
  return Curry._1(send, /* SetLaneValue */Block.__(3, [
                laneValue,
                index,
                value
              ]));
}

function onSetLength(send, laneValue, index) {
  return Curry._1(send, /* SetLoopAfterIndex */Block.__(1, [
                laneValue,
                index
              ]));
}

function make() {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              Curry._2(WebAudio$ReactTemplate.loadSound, "guitar.mp3", (function (buffer) {
                      self[/* state */1][/* soundBuffer */8][0] = /* Some */[buffer];
                      return /* () */0;
                    }));
              self[/* state */1][/* scheduler */7][0] = /* Some */[WebAudio$ReactTemplate.createSchedule((function (beatTime, beatLength) {
                        return Curry._1(self[/* send */3], /* Playback */Block.__(0, [
                                      beatTime,
                                      beatLength
                                    ]));
                      }))];
              return /* () */0;
            }),
          /* didUpdate */(function (param) {
              var newSelf = param[/* newSelf */1];
              if (param[/* oldSelf */0][/* state */1][/* isPlaying */6] !== newSelf[/* state */1][/* isPlaying */6]) {
                var match = newSelf[/* state */1][/* scheduler */7][0];
                if (match) {
                  var scheduler = match[0];
                  if (newSelf[/* state */1][/* isPlaying */6]) {
                    Curry._1(newSelf[/* send */3], /* ResetLanes */1);
                    return Curry._1(scheduler[/* start */0], /* () */0);
                  } else {
                    return Curry._1(scheduler[/* stop */1], /* () */0);
                  }
                } else {
                  return /* () */0;
                }
              } else {
                return 0;
              }
            }),
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              var partial_arg = self[/* send */3];
              var onSetValueBound = function (param, param$1, param$2) {
                return onSetValue(partial_arg, param, param$1, param$2);
              };
              var partial_arg$1 = self[/* send */3];
              var onSetLengthBound = function (param, param$1) {
                return Curry._1(partial_arg$1, /* SetLoopAfterIndex */Block.__(1, [
                              param,
                              param$1
                            ]));
              };
              var match = self[/* state */1][/* isPlaying */6];
              return React.createElement("div", {
                          className: "ma4"
                        }, React.createElement("div", undefined, React.createElement("button", {
                                  className: "w4",
                                  onClick: (function () {
                                      return Curry._1(self[/* send */3], /* SetPlayback */Block.__(2, [!self[/* state */1][/* isPlaying */6]]));
                                    })
                                }, match ? "Stop" : "Play")), React.createElement("div", {
                              className: "h1"
                            }), ReasonReact.element(/* None */0, /* None */0, Row$ReactTemplate.make("Octave", /* Octave */0, self[/* state */1][/* octave */0], onSetValueBound, onSetLengthBound, /* array */[])), React.createElement("div", {
                              className: "h1"
                            }), ReasonReact.element(/* None */0, /* None */0, Row$ReactTemplate.make("Transpose", /* Transpose */1, self[/* state */1][/* transpose */1], onSetValueBound, onSetLengthBound, /* array */[])), React.createElement("div", {
                              className: "h1"
                            }), ReasonReact.element(/* None */0, /* None */0, Row$ReactTemplate.make("Velocity", /* Velocity */2, self[/* state */1][/* velocity */2], onSetValueBound, onSetLengthBound, /* array */[])), React.createElement("div", {
                              className: "h1"
                            }), ReasonReact.element(/* None */0, /* None */0, Row$ReactTemplate.make("Chance", /* Chance */3, self[/* state */1][/* chance */3], onSetValueBound, onSetLengthBound, /* array */[])), React.createElement("div", {
                              className: "h1"
                            }), ReasonReact.element(/* None */0, /* None */0, Row$ReactTemplate.make("Offset", /* Offset */4, self[/* state */1][/* offset */4], onSetValueBound, onSetLengthBound, /* array */[])), React.createElement("div", {
                              className: "h1"
                            }), ReasonReact.element(/* None */0, /* None */0, Row$ReactTemplate.make("Length", /* Length */5, self[/* state */1][/* length */5], onSetValueBound, onSetLengthBound, /* array */[])));
            }),
          /* initialState */(function () {
              return /* record */[
                      /* octave */Lane$ReactTemplate.emptyLane(/* Octave */0),
                      /* transpose */Lane$ReactTemplate.emptyLane(/* Transpose */1),
                      /* velocity */Lane$ReactTemplate.emptyLane(/* Velocity */2),
                      /* chance */Lane$ReactTemplate.emptyLane(/* Chance */3),
                      /* offset */Lane$ReactTemplate.emptyLane(/* Offset */4),
                      /* length */Lane$ReactTemplate.emptyLane(/* Length */5),
                      /* isPlaying */false,
                      /* scheduler */[/* None */0],
                      /* soundBuffer */[/* None */0]
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              if (typeof action === "number") {
                if (action === 0) {
                  return applyToAllLanes(state, Lane$ReactTemplate.advance);
                } else {
                  return applyToAllLanes(state, Lane$ReactTemplate.reset);
                }
              } else {
                switch (action.tag | 0) {
                  case 0 : 
                      var beatTime = action[0];
                      var match = state[/* soundBuffer */8][0];
                      if (match) {
                        var buffer = match[0];
                        return /* SideEffects */Block.__(1, [(function (self) {
                                      var chance = Random.$$int(101);
                                      if (chance <= Lane$ReactTemplate.getValue(self[/* state */1][/* chance */3])) {
                                        var octave = Lane$ReactTemplate.getValue(self[/* state */1][/* octave */0]);
                                        var transpose = Lane$ReactTemplate.getValue(self[/* state */1][/* transpose */1]);
                                        var velocity = Lane$ReactTemplate.getValue(self[/* state */1][/* velocity */2]);
                                        var offset = Lane$ReactTemplate.getValue(self[/* state */1][/* offset */4]);
                                        var length = Lane$ReactTemplate.getValue(self[/* state */1][/* length */5]);
                                        var note = Caml_int32.imul(octave, 12) + transpose | 0;
                                        var gain = velocity / 100;
                                        var offsetValue = offset / 100;
                                        var durationValue = length / 100;
                                        Curry._6(WebAudio$ReactTemplate.playBuffer, buffer, note, gain, beatTime, offsetValue, durationValue);
                                      }
                                      return Curry._1(self[/* send */3], /* AdvancePlayback */0);
                                    })]);
                      } else {
                        return /* NoUpdate */0;
                      }
                  case 1 : 
                      var index = action[1];
                      return applyToLane(state, action[0], (function (subState) {
                                    return /* record */[
                                            /* values */subState[/* values */0],
                                            /* index */subState[/* index */1],
                                            /* visualIndex */subState[/* visualIndex */2],
                                            /* loopAfterIndex */index
                                          ];
                                  }));
                  case 2 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* octave */state[/* octave */0],
                                  /* transpose */state[/* transpose */1],
                                  /* velocity */state[/* velocity */2],
                                  /* chance */state[/* chance */3],
                                  /* offset */state[/* offset */4],
                                  /* length */state[/* length */5],
                                  /* isPlaying */action[0],
                                  /* scheduler */state[/* scheduler */7],
                                  /* soundBuffer */state[/* soundBuffer */8]
                                ]]);
                  case 3 : 
                      var value = action[2];
                      var index$1 = action[1];
                      return applyToLane(state, action[0], (function (subState) {
                                    Caml_array.caml_array_set(subState[/* values */0], index$1, value);
                                    return /* record */[
                                            /* values */subState[/* values */0],
                                            /* index */subState[/* index */1],
                                            /* visualIndex */subState[/* visualIndex */2],
                                            /* loopAfterIndex */Caml_primitive.caml_int_max(subState[/* loopAfterIndex */3], index$1)
                                          ];
                                  }));
                  
                }
              }
            }),
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

exports.component = component;
exports.applyToAllLanes = applyToAllLanes;
exports.applyToLane = applyToLane;
exports.onSetValue = onSetValue;
exports.onSetLength = onSetLength;
exports.make = make;
/* component Not a pure module */
