program CT;

{$AppType Console}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, untMain, untLevel, untEnemies, untGraphics, ui_unit, untObstacles,
  untItems, ui_inventory, ui_gamecamera, untWayPoint, untSkills, ui_skills,
  ui_scroll, untInteractiveObjects
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

