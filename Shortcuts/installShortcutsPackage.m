
(* If your firewall permits it you may install the Shortcuts` packge from a fresh Mathematica notebook like this: *)

(*
Import@"https://raw.githubusercontent.com/rolfmertig/Shortcuts/master/Shortcuts/installShortcutsPackage.m"
*)

(* If your firewall does not permit this, just download Shortcuts.zip  and install it to $UserBaseDirectory/Applications *)

(
URLSave[
	"https://raw.githubusercontent.com/rolfmertig/Shortcuts/master/Shortcuts.zip",
     FileNameJoin[{$TemporaryDirectory, "Shortcuts.zip"}]
];
ExtractArchive[FileNameJoin[{$TemporaryDirectory, "Shortcuts.zip"}], 
               FileNameJoin[{$UserBaseDirectory, "Applications"}] // Quiet;
];
Needs["Shortcuts`"]; Shortcuts`InstallShortcuts[];
)
