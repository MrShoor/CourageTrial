program CTRoomEditor;

//{$AppType Console}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, untRoomEditorMain, untRoomObstacles, runtimetypeinfocontrols
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfmrMain, fmrMain);
  Application.Run;
end.

