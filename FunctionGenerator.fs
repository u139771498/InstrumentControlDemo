namespace InstrumentControlDemo

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open NAudio.Wave
open InstrumentControlDemo

/// Function generator with audio output.
module FunctionGenerator =
    
    type private Waveform =
        | SineWave       of frequency : float<Hz> * amplitude : float32
        | FrequencySweep of start : float<Hz> * stop : float<Hz> * amplitude : float32 * duration : float<s>

    type private Command =
        | StartPlayback
        | StopPlayback
        | SetWaveform of waveform : Waveform
        | ReadSamples of count : int * chan : AsyncReplyChannel<float32 seq>

    type FunctionGenerator = private FunctionGenerator of agent : Agent<Command> * sampleRate : int<Hz>

    /// Returns a function which gives the nth sample of the specified function generator waveform.
    let private waveformFunc sampleInterval = function
        | SineWave (frequency, amplitude) -> 
            Function.sine amplitude frequency |> Function.sample sampleInterval
        | FrequencySweep (start, stop, amplitude, duration) -> 
            let chirpRate = (stop - start) / duration
            Function.chirp amplitude start chirpRate duration |> Function.sample sampleInterval
    
    /// Returns the wave provider for the function generator.
    let private waveProvider (agent : Agent<Command>) sampleRate =
        let readBytes (buffer : byte array) offset count =
            let samplesRequired = count / 4
            let samples = 
                agent.PostAndAsyncReply (fun chan -> ReadSamples(samplesRequired, chan))
                |> Async.RunSynchronously

            WaveBuffer.WriteFloat32Seq(buffer, offset, samples)
            samplesRequired * 4

        let format = WaveFormat.CreateIeeeFloatWaveFormat(int sampleRate, 1)

        { new IWaveProvider with
            member this.WaveFormat = format
            member this.Read(buffer, offset, count) = readBytes buffer offset count }
    
    /// Creates a function generator running at the specified sample rate which plays back waveforms
    /// over the given wave output.
    let create (sampleRate : int<Hz>) (waveOut : WaveOut) = 
        let sampleInterval = 1.0<s> / (float sampleRate)

        let agent = Agent.Start(fun mailbox ->
            
            /// handle a command when playback is off
            let rec waiting waveform = async {
                let! message = mailbox.Receive()
                match message with
                | StartPlayback         -> return! playing waveform 0
                | StopPlayback          -> return! waiting waveform
                | SetWaveform waveform' -> return! waiting waveform'
                | ReadSamples (count, chan) ->
                    chan.Reply <| seq { for _ in 0 .. count - 1 -> 0.0f }
                    return! waiting waveform }

            /// handle a command when playback is on
            and playing waveform sampleIndex = async {
                let! message = mailbox.Receive()
                match message with
                | StartPlayback         -> return! playing waveform 0
                | StopPlayback          -> return! waiting waveform
                | SetWaveform waveform' -> return! playing waveform' 0
                | ReadSamples (count, chan) ->
                    let f = waveformFunc sampleInterval waveform
                    chan.Reply <| seq { for n in 0 .. count - 1 -> f (sampleIndex + n) }
                    return! playing waveform (sampleIndex + count) }
              
            // initialise with playback off
            waiting (SineWave (100.0<Hz>, 0.25f)))
        
        /// initialise the wave output and return the initialised function generator
        waveProvider agent sampleRate |>  waveOut.Init
        waveOut.Play()
        FunctionGenerator(agent, sampleRate)

    /// Starts waveform playback on the given function generator.
    let startPlayback (FunctionGenerator(agent, _)) = agent.Post StartPlayback
    
    /// Stops waveform playback on the given function generator.
    let stopPlayback (FunctionGenerator(agent, _)) = agent.Post StopPlayback

    /// Sets the function generator to play back a waveform with the given parameters.
    let setSineWave frequency amplitude (FunctionGenerator(agent, _)) =
        SineWave (frequency, amplitude)
        |> SetWaveform
        |> agent.Post

    /// Sets the function generator to play back a frequency sweep with the given parameters.
    let setFrequencySweep start stop amplitude duration (FunctionGenerator(agent, _)) =
        FrequencySweep (start, stop, amplitude, duration)
        |> SetWaveform
        |> agent.Post