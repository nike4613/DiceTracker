﻿// $begin{copyright}
//
// Copyright (c) 2018 IntelliFactory and contributors
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

/// Bindings for Ace Editor. https://ace.c9.io/
module DiceTracker.Website.Ace

open System.Threading.Tasks
open FSharp.Compiler.Diagnostics
open Microsoft.JSInterop

/// Highlight a warning or an error.
/// This combines an Ace Marker (highlight a range of code)
/// and an Ace Annotation (icon in the margin with text on hover).
type Annotation =
    {
        /// The base-0 line of the start of the annotation.
        row: int
        /// The base-1 column of the start of the annotation.
        column: int
        /// The base-0 line of the end of the annotation.
        y2: int
        /// The base-1 column of the end of the annotation.
        x2: int
        /// The message displayed on hover.
        text: string
        /// "warning" or "error".
        ``type``: string
    }

/// Set the currently displayed annotations.
let SetAnnotations (js: IJSInProcessRuntime) (messages: FSharpDiagnostic[]) =
    let annotations = messages |> Array.map (fun info ->
        {
            row = info.StartLine - 1
            column = info.StartColumn
            y2 = info.EndLine - 1
            x2 = info.EndColumn
            text = info.Message
            ``type`` =
                match info.Severity with
                | FSharpDiagnosticSeverity.Warning -> "warning"
                | FSharpDiagnosticSeverity.Error -> "error"
                | FSharpDiagnosticSeverity.Hidden -> "hidden"
                | FSharpDiagnosticSeverity.Info -> "info"
        }
    )
    js.InvokeVoid("DiceTracker.setAnnotations", annotations)

/// Focus the editor and select the code range of the given message.
let SelectMessage (js: IJSInProcessRuntime) (info: FSharpDiagnostic) =
    js.InvokeVoid("DiceTracker.selectRange",
        info.StartLine - 1, info.StartColumn,
        info.EndLine - 1, info.EndColumn
    )