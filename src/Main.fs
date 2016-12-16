module CommonLibrary =

    // the two-track type
    type Result<'TSuccess,'TFailure> =
        | Success of 'TSuccess
        | Failure of 'TFailure

    // convert a single value into a two-track result
    let succeed x =
        Success x

    // convert a single value into a two-track result
    let fail x =
        Failure x

    // appy either a success function or failure function
    let either successFunc failureFunc twoTrackInput =
        match twoTrackInput with
        | Success s -> successFunc s
        | Failure f -> failureFunc f


    // convert a switch function into a two-track function
    let bind f =
        either f fail

    // pipe a two-track value into a switch function
    let (>>=) x f =
        bind f x

    // compose two switches into another switch
    let (>=>) s1 s2 =
        s1 >> bind s2

    // convert a one-track function into a switch
    let switch f =
        f >> succeed

    // convert a one-track function into a two-track function
    let map f =
        either (f >> succeed) fail

    // convert a dead-end function into a one-track function
    let tee f x =
        f x; x

    // convert a one-track function into a switch with exception handling
    let tryCatch f exnHandler x =
        try
            f x |> succeed
        with
        | ex -> exnHandler ex |> fail

    // convert two one-track functions into a two-track function
    let doubleMap successFunc failureFunc =
        either (successFunc >> succeed) (failureFunc >> fail)

    // add two switches in parallel
    let plus addSuccess addFailure switch1 switch2 x =
        match (switch1 x),(switch2 x) with
        | Success s1,Success s2 -> Success (addSuccess s1 s2)
        | Failure f1,Success _  -> Failure f1
        | Success _ ,Failure f2 -> Failure f2
        | Failure f1,Failure f2 -> Failure (addFailure f1 f2)

module DomainTypes =

    open System
    open CommonLibrary

    type XrmMessage =
        | Create
        | Update
        | Delete

    type XrmRunningContext =
        | CallingUser
        | SpecificUser of Guid

    // type XrmPluginStepExecutionOrder int

    type XrmImageType =
        | PreImage
        | PostImage
        | Both

    type XrmAttributesCollection =
        | AllAttributes
        | SpecificAttributes of seq<string>

    type XrmPluginImage =
        {
            StepId: Guid
            ImageId: Guid
            Exists: bool
            ImageType: XrmImageType
            ImageAlias: string
            IncludedAttributes: XrmAttributesCollection
            ImageName: string
        }

    type XrmPluginStepExecutionStage =
        | PreValidation
        | PreOperation
        | PostOperation

    type XrmPluginStepExecutionMode =
        | Asynchronous
        | Synchronous

    type XrmPluginStepDeployment =
        | Server
        | Offline

    type XrmPluginStep =
        {
            PluginId: Guid
            StepId: Guid
            Exists: bool
            Images: seq<XrmPluginImage>
            Message: XrmMessage
            PrimaryEntity: string
            SecondaryEntity: string
            FilteringAttributes: XrmAttributesCollection
            // EventHandler:
            StepName: string
            RunInUserContext: XrmRunningContext
            ExecutionOrder: int
            Description: string
            EventPipelineExecutionStage: XrmPluginStepExecutionStage
            Deployment: XrmPluginStepDeployment
            UnsecureConfiguration: string
            SecureConfiguration: string
        }

    type XrmPlugin =
        {
            // assembly to register to CRM
            PluginAssembly: System.Reflection.Assembly
            PluginId: Guid
            Exists: bool
            PluginSteps: seq<XrmPluginStep>
        }

module Logger =

    open CommonLibrary
    open DomainTypes

    let log twoTrackInput =
        let success x = printfn "DEBUG. Success so far: %A" x; x
        let failure x = printfn "ERROR. %A" x; x
        doubleMap success failure twoTrackInput

module Validation =

    open CommonLibrary
    open DomainTypes

    // create a "plus" function for validation functions
    let (&&&) v1 v2 =
        let addSuccess  r1 r2 = r1 // return first
        let addFailure s1 s2 = s1 + "; " + s2 // concat
        plus addSuccess addFailure v1 v2

    let combinedValidation =
        () // maybe have each type validate itself?

module XrmPluginDeployment =

    open CommonLibrary
    open DomainTypes


[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
