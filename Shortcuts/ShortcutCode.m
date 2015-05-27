(* Wolfram Language package *)
Shortcut::usage = "Shortcut[name] contains the kernel definition for the shortcut name. For a list of shortcuts type ?Shortcuts";

Shortcuts::usage = ToString[Style["In the following table the " <>
                  Switch[$OperatingSystem,
                  	     "Windows",
                  	     "\[CommandKey] key is meant to with the Windows key next to the \[ControlKey] key. \n\n",
                  	     _, 
                  	     ""
                  ], "Text", FontSize -> 18]] <>
Switch[
	$OperatingSystem,
	"Windows",                   
(* Windows *)	
 StringReplace[#, {"Ctrl" -> "\[ControlKey]", "Cmd" -> "\[CommandKey]", "Alt" -> "\[AltKey]", "Shift" -> "\[ShiftKey]", "Tab" -> "\[TabKey]"}]&@
 ToString[#, StandardForm] &@
  With[{os = 
     Switch[$OperatingSystem, "MacOSX", "Macintosh", "Windows", 
      "Windows", "Unix", "X"]},
   With[{
     myjokerdir2 = FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", 
        "TextResources", os}]},
    Grid[{
    	  {"Ctrl Shift \[UpArrow] ",   "Select all cells from the beginning of the notebook until the  cursor position."}, 
    	  {"Ctrl Shift \[DownArrow] ", "Select all cells from the position of the mouse until the end of the notebook."}, 
    	  {"Ctrl Shift PageUp ", "Evaluate all cells from the beginning of the notebook until the position of the mouse."}, 
    	  {"Ctrl Shift X ", "Delete all generated cells, like Output, Message and Print cells, also in MessagesNotebook[]."}, 
    	  {"Ctrl Alt X ", "Delete all non-Input and non-Code cells "}, 
    	  {"Ctrl Q ", "Quit and restart Kernel "}, 
    	  {"Ctrl R", "Quit and restart the FrontEnd; all Untitled notebooks are closed and others saved. Reopening the selected notebook if it is saved "}, 
    	  {"Ctrl ` ", "Evaluate Notebook"}, 
    	  {"Ctrl H ", "Evaluate Notebook"}, 
    	  {"Ctrl Tab ", "Delete all output and evaluate all cells from the beginning  of the notebook until the cursor position "}, 
    	  {"Ctrl Shift Tab ", "Delete all output, restart the kernel and evaluate all cells from the beginning of the notebook until the cursor position "}, 
          {"Ctrl Shift ,", "Copy/Evaluate/Paste the selected cell subexpression into a new notebook "}, 
          {"Ctrl Shift X ", "Delete all generated cells, like Output, Message and Print cells "}, 
          {"Cmd Alt B ", "Move the cursor from anywhere inside a cell to the cell bracket "}, 
          {"Cmd Alt M ", "Minimize all Mathematica notebooks "}, 
          {"Cmd Alt U ", "Cut the selected cell and paste it before the preceding cell "}, 
          {"Cmd Alt D ", "Cut the selected cell and paste it after the following cell "}, 
          {"Ctrl Shift Delete ", "Close all Untitled-*  notebooks without confirmation "}, 
          {"F4 ", "Insert [[]]"}, 
          {"F6 ", "Apply SetOptions[SelectedNotebook[], WindowMargins -> 42]"}, 
          {"Ctrl T ", "Evaluate the file joker.m from " <> myjokerdir2}, 
          {"Ctrl Shift J ", "Open joker.m from " <> myjokerdir2}, 
          {Framed[ Style["The following three shortcuts work only on english  keyboards: ", "Text"], FrameStyle -> None, FrameMargins -> 10], SpanFromLeft}, 
          {"Ctrl [ "(*]*), "Insert [["}, {(*[*)"Ctrl ] ", "Insert ]]"}, 
          {(*[*)"Ctrl Alt ] ", "Insert [[]]"}}, Alignment -> {Left}, Dividers -> All, 
          BaseStyle -> {FontSize -> 18},
          FrameStyle -> LightGray]]
   ]
     ,
(* ************************************************************ *)     
(* Linux *)
"Unix",
""
];

Begin["`Private`"]

Shortcut["SelectCellBracket"] :=
	( "Info: from anywhere inside a cell select the cell brack; like continuous Ctrl .";
	  SelectionMove[SelectedNotebook[], All, Cell]
	);


Shortcut["Minimize"] :=
    ( "Info: this minimizes all windows ";
      FrontEndExecute[ {FrontEndToken[#1, "WindowMiniaturize"]}]& /@ Notebooks[] 
    );
    
    
Shortcut["DeleteOutputAndMessages"] :=
    ( "Info: Delete all output (including Print cells) in the selected notebook and contents in the Messages notebook ";
      FrontEndTokenExecute @ "DeleteGeneratedCells";
      NotebookDelete @ Cells[ CellStyle -> "Print"];
      NotebookDelete @ Cells @ MessagesNotebook[];
    );


Shortcut["DeleteAllCellsButInputAndCode"] :=
    ( "Info: Delete all cells but Input and Code (including Print cells) in the selected notebook and contents in the Messages notebook ";
    FrontEndTokenExecute @ "DeleteGeneratedCells";
    NotebookDelete @ Cells[ CellStyle -> (Alternatives@@Complement[ Union@Flatten@Experimental`CellStyleNames[], {"Input", "Code"}])];
    NotebookDelete @ Cells @ MessagesNotebook[];
    );


Shortcut["RestartFrontEnd", enb_:EvaluationNotebook[]] :=
    ( "Info: restart the FrontEnd including closing all Untitled notebooks and potentially repoen the already saved notebook from 
             where this shortcut is executed";
    Block[{reopenfilename = "", StartTime, ToFileTime, Id, mproc},
    (* the restarting is done throu the OS. On MacOSX and Linux there needs to be a grace time *)
	With[{wait = "0.5" (* seconds *)}, (* this is heuristically determined! You may have to change this on you computer! *)
		If[ ("FileName" /. NotebookInformation[enb]) === "FileName"
			,
			reopenfilename = "";
    	    If[$OperatingSystem === "Unix",
     		(* since othwerwise on Linux restarting does not work ... , do: *)
			NotebookSave[enb, FileNameJoin[{$TemporaryDirectory, "shortcuttmp.nb"}]]
    	    ]
			,
			reopenfilename = "\"" <> ToFileName["FileName" /. NotebookInformation[enb]] <> "\""
		];
	(* close all Untitled notebooks : *)
	Map[NotebookClose, Select[Notebooks[], ReplaceAll["FileName",NotebookInformation[#]] === "FileName"&]];
	(* save all left notebooks except the Messages one: *)
	Map[NotebookSave,  Select[Notebooks[],Replace["WindowTitle",NotebookInformation[#]]=!="Messages"&]];
	(* kill the newest FrontEnd operating system depently: *)
	Switch[ $OperatingSystem ,
	"Windows"
    	,    (* use NETLink to get a list of PID's and take the last one to be killed *)
		Needs["NETLink`"]; Symbol["NETLink`NETNew"]["System.Diagnostics.Process"];
		mproc = Select[System`Diagnostics`Process`GetProcessesByName["Mathematica"], Quiet[#@StartTime]=!=$Failed &];
		ReadList["!taskkill /PID " <> 
		ToString[(Last@SortBy[mproc, 
		(* if there are processes by other users we might get Access Denied, so we skip them: *)	
		#@StartTime @ ToFileTime[]&] ) @ Id ] <> " /f & start \"\" \"" <> 
		FileNameJoin[{$InstallationDirectory, "Mathematica.exe"}] <> "\" " <> reopenfilename
		  ,String];
    	,
	"Unix"
	    , 
		Run["kill -9 " <> ToString[ Last[ Sort[ ReadList[ StringToStream[RunProcess[{"pidof", "Mathematica"}, "StandardOutput"]],Number]]] ] <>
        	";  rm -f /tmp/shortcuttmp.nb ;sleep "<>wait<>"; '" <>  
        	FileNameJoin[{$InstallationDirectory, "Executables", "Mathematica"}]  <> "' " <> reopenfilename <> "& " 
    	]
		,
	"MacOSX"
		,
		Run["kill -9 " <> 
          (* take the newest pid , without using awk : *)
          ToString[ Last[Import["!ps axc | grep Mathematica", "Table"][[All, 1]]] ] <>
          "; sleep "<>wait<>"; open -a " <> FileNameTake[ParentDirectory[$InstallationDirectory], -1]  <> " " <> reopenfilename
		]
(*Switch*)
	]
]; (* With *)
] (* Block*)
);


Shortcut["RestartKernel"] :=
(
                    With[{ enb = SelectedNotebook[],
                             currentSetting = ToString@CurrentValue[$FrontEnd, "ClearEvaluationQueueOnKernelQuit"] },
                       Composition[
                              FrontEndExecute[FrontEndToken[#, "EvaluateNotebook"] ]&
                              ,
                              CreateDocument[#, Visible -> If[$OperatingSystem === "Windows",False, True], WindowSize -> {50, 50}, 
                                                WindowMargins -> {{-500, -500}, {-500, -500}},
                                                WindowTitle -> "restart"
                              ] &
                              ,
                              Map[ Function[z, Cell[StringReplace[z, "CURSET" -> currentSetting], "Input"]], #] &
                      ][
                      {
                      "CurrentValue[$FrontEndSession, \"ClearEvaluationQueueOnKernelQuit\"] = False; Quit[];"
                      ,
                       "CurrentValue[$FrontEndSession, \"ClearEvaluationQueueOnKernelQuit\"] = CURSET; NotebookClose[EvaluationNotebook[]]; $Line = 0; "
                      }
                      ] ;
                      SetSelectedNotebook[enb];
                     ]
);



(* So this is a slight rewrite of Kuba's answer here: http://mathematica.stackexchange.com/a/55073/29
   However, it is not the same, which becomes apparent when evaluatin a larger notebook 

Shortcut["EvalFromTop"] :=  Module[{i, enb = EvaluationNotebook[]},
  (* allow the cursor to be in between cells :*)
  If[CurrentValue[enb, "CellCount"] === 0, SelectionMove[enb, Previous, Cell, AutoScroll -> False]; ];
  (* Only go on if we are not at the top of the notebook: *)
  If[CurrentValue[enb, "CellCount"] > 0,
  	(* this **DOES NOT WORK*** , while it clearly should 
       SelectionMove[enb, All, Cell];
       Therefore this will all not work if the cursor is inside a cell ...
   *)
   Do[ SelectionMove[Experimental`FromCellIndex[enb, i], All, Cell, AutoScroll -> False];
       SelectionEvaluate[enb];
       ,{i, 1, Experimental`ToCellIndex @ SelectedCells[enb][[1]]}
   ];
   (* move explicitly out of the cell, otherwise the last cell might be selected ... *)
  If[CurrentValue[enb, "CellCount"] === 1, SelectionMove[enb, After, Cell, AutoScroll -> False]; ];
  ]];
*)
 
 
Shortcut["EvalFromTop"] :=  ( Shortcut["SelectToTop"]; SelectionEvaluate[EvaluationNotebook[]] );
  
(* Notice that from http://mathematica.stackexchange.com/a/55073/29
   I could not find a way to actually just select all cells,
   so I need to keep doing it the old way with CellTags and looping, 
   until the question gets truly answered. 
 *)
 
SetAttributes[Shortcut, HoldRest];
Shortcut["SelectToTop", lastCellFunction_: (CellEvaluationFunction :> Identity)] :=
            Module[{ enb = EvaluationNotebook[],
                     tag = StringJoin["tmp", ToString[Round[AbsoluteTime[]/$TimeUnit]]]
                   },
                If[ And[(enb =!= $Failed), Length[Cells[enb]] > 0 
                ],
               (*Iff the cursur is in between cells, move up one cell (with thank's to Kuba for the CellCount information) : *)
                If[CurrentValue[enb, "CellCount"] === 0,
                   SelectionMove[enb, Previous, Cell, AutoScroll -> False];
                ];
                (* Only go on if we are not at the top of the notebook: *)
                If[CurrentValue[enb, "CellCount"] > 0, (* add the unique tag to the last cell: *)
(*SelectionMove[enb, All, Cell, AutoScroll -> False];*)
                   If[lastCellFunction =!= (CellEvaluationFunction :> Identity),
                      SetOptions[NotebookSelection[enb], lastCellFunction]
                   ];
                   MathLink`CallFrontEnd[FrontEnd`SelectionAddCellTags[enb, {tag}]];
                   SelectionMove[enb, Before, Notebook, AutoScroll -> False];
                   SelectionMove[enb, Next, Cell, AutoScroll -> False];
                   (* add the cell tag to all cells up to the last one which we tagged above *)
                   While[FreeQ[CellTags /. Options[NotebookSelection[enb]], tag],
                         MathLink`CallFrontEnd[FrontEnd`SelectionAddCellTags[enb, {tag}]];
                         SelectionMove[enb, Next, Cell, AutoScroll -> False];
                ];
                (* select all cells with tag *)
                NotebookFind[enb, tag, All, CellTags, AutoScroll -> False];
                ];
                (* remove the tags *)
                MathLink`CallFrontEnd[FrontEnd`SelectionRemoveCellTags[enb, {tag}]];
            ]
            ];
   
Shortcut["SelectToBottom"] := 
	       Module[{ enb = EvaluationNotebook[],
                   tag = StringJoin["ShortcutsTemp", ToString[Round[AbsoluteTime[]/$TimeUnit]]]
                   },
                If[
                   (enb =!= $Failed) && Length[Cells[enb]] > 0,
               (*Iff the cursur is in between cells, move down one cell (with thank's to Kuba for the CellCount information) : *)
                If[SameQ[CurrentValue[enb, "CellCount"] ,0],
                   SelectionMove[enb, Next, Cell, AutoScroll -> False]
                ];
                (* Only go on if we are not at the end of the notebook: *)
                If[CurrentValue[enb, "CellCount"] > 0,
                   (* add the unique tag to the last cell: *)
                   MathLink`CallFrontEnd[FrontEnd`SelectionAddCellTags[enb, {tag}]];
                   (* and move to the end of the notebook: *)
                   SelectionMove[enb, After, Notebook, AutoScroll -> False];
                   (* on to the last cell *)
                   SelectionMove[enb, Previous, Cell, AutoScroll -> False];
                   (* add the cell tag to all cells up to the first one which we tagged above *)
                   While[FreeQ[ReplaceAll[CellTags, Options[NotebookSelection[enb]]], tag],
                         MathLink`CallFrontEnd[FrontEnd`SelectionAddCellTags[enb, {tag}]];
                         SelectionMove[enb, Previous, Cell, AutoScroll -> False];
                ];
                (* select all cells with tag *)
                NotebookFind[enb, tag, All, CellTags, AutoScroll -> False];
                ];
                (* remove the tags *)
                MathLink`CallFrontEnd[FrontEnd`SelectionRemoveCellTags[enb, {tag}]];
            ]
            ]
            
Shortcut["CloseUntitledNotebooks"] := 
        Map[NotebookClose, 
        	Select[Notebooks[],
                   StringMatchQ[ "WindowTitle" /. NotebookInformation[#], "Untitled-*"]&]];
                   
(* http://mathematica.stackexchange.com/questions/73212/how-to-move-a-cell-up-or-down-with-a-keystroke *)                   


Shortcut["MoveCellsUp"] := 
  With[{nb = SelectedNotebook[]},
   (NotebookWrite[nb, #2];
      SelectionMove[nb, Previous, Cell]
   ) &[
    SelectionMove[nb, All, Cell],
    NotebookRead[SelectedCells[nb]],
    NotebookDelete[nb],
    SelectionMove[nb, #, Cell] & /@ {Previous, Before}
    ]
  ];
  
Shortcut["MoveCellsDown"] :=  
  With[{nb = SelectedNotebook[]},
   (NotebookWrite[nb, #2];
      SelectionMove[nb, Previous, Cell]) &[
    SelectionMove[nb, All, Cell],
    NotebookRead[SelectedCells[nb]],
    NotebookDelete[nb],
    SelectionMove[nb, #, Cell] & /@ {Next, After}
    ]
  ];
  

  (*
status[s__]:= SetOptions[EvaluationNotebook[], WindowStatusArea -> StringJoin[ToString[#,InputForm]&/@{s}]];
Shortcut["DoIt"] := (
	status[DateString[]," start Deleting Output / Print / Message cells"];
	NotebookDelete @ Cells[ CellStyle -> ("Output"|"Print"|"Message")]; (* usually this makes sense *)
	NotebookDelete @ Cells @ MessagesNotebook[]; (* and this too *)
	status[DateString[], " Selecting all cells upewards."];
	If[("FileName" /. NotebookInformation[EvaluationNotebook[]]) =!= "FileName", 
		Shortcut["SelectToTop", CellEvaluationFunction :> ( (
	    NotebookWrite[EvaluationCell[], (NotebookRead[EvaluationCell[]] /. (CellEvaluationFunction :> _ ) :> Sequence[]), All ];
	    SelectionEvaluateCreateCell[EvaluationNotebook[]];
		NotebookSave[EvaluationNotebook[]]; 
		)&)
		],
		Shortcut["SelectToTop"]
	];
	status["restart the kernel."];
	Shortcut["RestartKernel"]; (* restart the kernel *)
	status[DateString[]<> " Evaluate the selected cells."];
	SelectionEvaluate[EvaluationNotebook[]]; (* evaluate all *)
	status[DateString[]];
);
*)


Shortcut["TestRun1"] := (
	NotebookDelete @ Cells[ CellStyle -> ("Output"|"Print"|"Message")]; (* usually this makes sense *)
	NotebookDelete @ Cells @ MessagesNotebook[]; (* and this too *)
	Shortcut["SelectToTop"];   (* select all cells from the cursor up to the beginning *)
	SelectionEvaluate[EvaluationNotebook[]]; (* evaluate all selected *)
);



Shortcut["TestRun2"] := (
	NotebookDelete @ Cells[ CellStyle -> ("Output"|"Print"|"Message")]; (* usually this makes sense *)
	NotebookDelete @ Cells @ MessagesNotebook[]; (* and this too *)
	Shortcut["SelectToTop"];   (* select all cells from the cursor up to the beginning *)
	Shortcut["RestartKernel"]; (* restart the kernel *)
	SelectionEvaluate[EvaluationNotebook[]]; (* evaluate all selected *)
);



End[];
