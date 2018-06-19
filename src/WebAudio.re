type audioContext;
type audioParam;
type node;

[@bs.new] external create_audioContext : unit => audioContext = "AudioContext";
[@bs.get] external getCurrentTime : (audioContext) => float = "currentTime";
[@bs.get] external getDestination : (audioContext) => node = "destination";
[@bs.send] external createOscillator : (audioContext) => node = "createOscillator";
[@bs.get] external oscFrequency : (node) => audioParam = "frequency";
[@bs.send] external oscStart : (node, float) => unit = "start";
[@bs.send] external oscStop : (node, float) => unit = "stop";
[@bs.set] external oscType : (node, string) => unit = "type";
[@bs.send] external connect : (node, node) => unit = "connect";
[@bs.set] external setValue: (audioParam, float) => unit = "value";

let audioContext = create_audioContext();

type scheduler = {
  beatTime: ref(float),
  timeoutId: ref(Js.Global.timeoutId)
};

let createSchedule = (callback) => {
  let beatTime = ref(0.);
  let bpm = 120.;
  let ticksPerBeat = 1.;
  let beatLength = 60. /. bpm /. ticksPerBeat;

  let rec onTimeout = () => {
    let targetTime = getCurrentTime(audioContext) +. 0.2;

    while (beatTime^ < targetTime) {
      callback(beatTime, beatLength);

      beatTime := beatTime^ +. beatLength;
    };

    Js.Global.setTimeout(onTimeout, 100) |> ignore;
  };

  onTimeout();
};
