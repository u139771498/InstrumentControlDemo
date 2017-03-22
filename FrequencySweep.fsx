#r "System.Windows.Forms.DataVisualization"
#r "../packages/NAudio.1.7.3/lib/net35/NAudio.dll"
#r "../packages/FSharp.Charting.0.90.13/lib/net40/FSharp.Charting.dll"
#r "../packages/Rx-Core.2.2.5/lib/net45/System.Reactive.Core.dll"
#r "../packages/Rx-Interfaces.2.2.5/lib/net45/System.Reactive.Interfaces.dll"
#r "../packages/Rx-Linq.2.2.5/lib/net45/System.Reactive.Linq.dll"
#r "../packages/FSharp.Control.Reactive.3.2.0/lib/net40/FSharp.Control.Reactive.dll"

#load "Util.fs"
#load "FunctionGenerator.fs"
#load "LockInAmplifier.fs"

open FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharp.Control.Reactive
open FSharp.Charting
open NAudio.Wave
open InstrumentControlDemo

let lockin = LockInAmplifier.create 16000<Hz> (new WaveInEvent())
let gen = FunctionGenerator.create 16000<Hz> (new WaveOut())

/// Measures lock-in output over a function generator frequency sweep.
Async.Start <| async {
    do! Async.Sleep 1000

    lockin |> LockInAmplifier.setReferenceFrequency 500.0<Hz>
    lockin |> LockInAmplifier.setTimeConstant 0.1<s>
    lockin |> LockInAmplifier.startRecording
    
    gen |> FunctionGenerator.setFrequencySweep 400.0<Hz> 600.0<Hz> 1.0f 30.0<s>
    gen |> FunctionGenerator.startPlayback
    do! Async.Sleep 30000
        
    gen |> FunctionGenerator.stopPlayback
    lockin |> LockInAmplifier.stopRecording }

lockin |> LockInAmplifier.output
|> Observable.mapi (fun i x -> (i, x))
|> LiveChart.FastLineIncremental
|> Chart.WithXAxis(MajorGrid = ChartTypes.Grid(Enabled = false))
|> Chart.WithYAxis(MajorGrid = ChartTypes.Grid(Enabled = false))
|> Chart.Show