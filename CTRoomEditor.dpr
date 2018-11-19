program CTRoomEditor;

//{$AppType Console}

uses
  Forms,
  untroomeditormain in 'untroomeditormain.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfmrMain, fmrMain);
  Application.Run;
end.

