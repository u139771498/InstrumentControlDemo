namespace InstrumentControlDemo

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharp.Control.Reactive
open NAudio.Wave
open InstrumentControlDemo

/// Audio lock-in amplifier.
module LockInAmplifier =
    
    type private FilterSettings =
        { mutable ReferenceFrequency : float<Hz>
          mutable TimeConstant       : float<s>
          Sync                       : obj }

    type private Command =
        | StartRecording
        | StopRecording
        | SetReferenceFrequency of frequency : float<Hz>
        | SetTimeConstant of timeConstant : float<s>
        
    type LockInAmplifier = private LockInAmplifier of agent : Agent<Command> * output : IObservable<float32>

    // functions for reading and changing filter settings (thread-safe)
    let private getFilterFrequency filter   = lock filter.Sync (fun () -> filter.ReferenceFrequency)
    let private setFilterFrequency f filter = lock filter.Sync (fun () -> filter.ReferenceFrequency <- f)
    let private getFilterTimeConstant filter    = lock filter.Sync (fun () -> filter.TimeConstant)
    let private setFilterTimeConstant tc filter = lock filter.Sync (fun () -> filter.TimeConstant <- tc)
    let private getFilterCoeff sampleInterval filter = 
        let tc = getFilterTimeConstant filter 
        float32 <| Math.Pow(1.0 / Math.E, 1.0 / (tc / sampleInterval))

    /// Creates a lock-in amplifier which processes the signal from the given wave input.
    let create (sampleRate : int<Hz>) (recorder : WaveInEvent) =
        recorder.WaveFormat <- WaveFormat.CreateIeeeFloatWaveFormat(int sampleRate, 1)
        let sampleInterval = 1.0<s> / (float sampleRate)

        // initial filter settings
        let filter = 
            { ReferenceFrequency = 500.0<Hz>
              TimeConstant = 0.1<s>
              Sync = new obj() }

        let agent = Agent.Start(fun mailbox ->
            let rec loop () = async {
                let! message = mailbox.Receive()
                match message with
                | StartRecording          -> recorder.StartRecording()
                | StopRecording           -> recorder.StopRecording()
                | SetReferenceFrequency f -> filter |> setFilterFrequency f
                | SetTimeConstant tc      -> filter |> setFilterTimeConstant tc
                
                return! loop () }
                
            loop ())

        // process the recorded samples to produce the lock-in output
        let output =
            recorder.DataAvailable
            // for every float32 sample in the buffer
            |> Observable.flatmapSeq (fun bytes -> Seq.ofArray <| bytes.AsFloat32Array())
            // branch the signal into two parts which will be processed independently
            |> Observable.tee
            // mix the two branches samples with a sine and cosine wave respectively at the reference frequency
            |> Observable.mixPair
                (fun () -> filter |> (getFilterFrequency >> Function.sine   1.0f >> Function.sample sampleInterval))
                (fun () -> filter |> (getFilterFrequency >> Function.cosine 1.0f >> Function.sample sampleInterval))
            // low-pass filter both branches
            |> Observable.lowPassPair
                (fun () -> filter |> getFilterCoeff sampleInterval)
            // decimate to 20 samples per second
            |> Observable.decimate ((int sampleRate) / 20)
            // and compute the vector magnitude of the two signals
            |> Observable.magnitude

        LockInAmplifier (agent, output)

    /// Returns the output signal for the lock-in amplifier.
    let output (LockInAmplifier (_, output)) = output

    /// Starts processing input and generating output from the lock-in amplifier.
    let startRecording (LockInAmplifier (agent, _)) = StartRecording |> agent.Post

    /// Stops processing input and generating output from the lock-in amplifier.
    let stopRecording (LockInAmplifier (agent, _)) = StopRecording |> agent.Post

    /// Sets the reference frequency of the lock-in amplifier.
    let setReferenceFrequency f (LockInAmplifier (agent, _)) = SetReferenceFrequency f |> agent.Post

    /// Sets the time constant of the lock-in amplifier.
    let setTimeConstant tc (LockInAmplifier (agent, _)) = SetTimeConstant tc |> agent.Post