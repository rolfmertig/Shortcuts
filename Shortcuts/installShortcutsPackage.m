

(* If your firewall permits it you may install the Shortcuts` packge from a fresh Mathematica notebook like this: *)

(*
Import@"https://raw.githubusercontent.com/rolfmertig/Shortcuts/master/Shortcuts/installShortcutsPackage.m"
*)

Import["https://raw.githubusercontent.com/rolfmertig/MathematicaPackageInstall/master/MathematicaPackageInstall/MathematicaPackageInstall.m"]; 
MathematicaPackageInstall`MathematicaPackageInstall["Shortcuts`"]; 
Needs["Shortcuts`"]; 
Shortcuts`InstallShortcuts[]