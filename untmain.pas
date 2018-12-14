unit untMain;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, untLevel, avBase, avRes, avTypes, mutils,
  avCanvas, avMiniControls,
  untFloor, ui_start_menu,
  bWorld;

type

  { TUPSObject }

  TUPSObject = class(TavObject)
  private
    FStartTitle: TavmCustomControl;
    FFloor: TFloorMap;
    FWorld: TbWorld;
  protected
    procedure EMUps(var msg: TavMessage); message EM_UPS;
  public
    procedure SetState(AWorld: TbWorld; AFloor: TFloorMap; AStartTitle: TavmCustomControl);
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private
    FStartTitle: TamvStartTitle;

    FMain: TavMainRender;
    FDefFBO: TavFrameBuffer;

    FUPSObj: TUPSObject;

    FFPSLastSec  : Integer;
    FFPSLastCount: Integer;
    FFPSCounter  : Integer;

    FWorld: TbWorld;
    FWaitForNewGame: Boolean;
    FFloor: TFloorMap;

    FWaitForExit: Boolean;

    procedure AutoInit;
    procedure RenderScene;

    procedure InitWorld;
    procedure OnAfterWorldDraw(Sender: TObject);

    procedure MakeNewGameIfNeeded();
    procedure MakeNewGame_Delayed(ASender: TObject);
    procedure DoExit_Delayed(ASender: TObject);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  Math;

{$R *.lfm}

{ TUPSObject }

procedure TUPSObject.EMUps(var msg: TavMessage);
  function NeedClipCursor(): Boolean;
  begin
    if FFloor <> nil then
      if FFloor.UI.InGameMenuVisible then Exit(False);
    if FStartTitle <> nil then
      if FStartTitle.Visible then Exit(False);
    Result := GetForegroundWindow = FWorld.Main.Window;
  end;
var
  i: Integer;
  rct: TRect;
begin
  if NeedClipCursor() then
  begin
    ZeroClear(rct, SizeOf(rct));
    GetClientRect(FWorld.Main.Window, rct);
    ClientToScreen(FWorld.Main.Window, rct.TopLeft);
    ClientToScreen(FWorld.Main.Window, rct.BottomRight);
    ClipCursor(@rct);
  end
  else
    ClipCursor(nil);
  FWorld.UpdateStep(msg.param);
  for i := 0 to min(msg.param, 8) - 1 do
  begin
    FFloor.CurrentRoom.UpdateStep();
  end;
end;

procedure TUPSObject.SetState(AWorld: TbWorld; AFloor: TFloorMap; AStartTitle: TavmCustomControl);
begin
  FStartTitle := AStartTitle;
  FFloor := AFloor;
  FWorld := AWorld;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Randomize;
  FMain := TavMainRender.Create(nil);
  FMain.Camera.At := Vec(0,0,0);
  FMain.Camera.Up := Vec(0,1,0);
  FMain.Camera.Eye := FMain.Camera.At + Vec(10, 10, 5);

  FDefFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f], [True, False]);

  FStartTitle := TamvStartTitle.Create(FMain);
  FStartTitle.OnNewGame := {$IfDef FPC}@{$EndIf}MakeNewGame_Delayed;
  FStartTitle.OnExit := {$IfDef FPC}@{$EndIf}DoExit_Delayed;

  InitWorld;
end;

procedure TfrmMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  if FWaitForExit then
  begin
    Close;
    Exit;
  end;

  if FMain.Inited3D then
	  FMain.InvalidateWindow;
  Done := False;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FFloor <> nil then
  begin
    if Key = VK_ESCAPE then
    begin
      FFloor.UI.InGameMenuVisible := not FFloor.UI.InGameMenuVisible;
    end
    else
    begin
      FFloor.CurrentRoom.KeyPress(Key);
      if Key = Ord(' ') then
        FWorld.Renderer.InvalidateShaders;
    end;
  end;
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var btnIdx: Integer;
begin
  case Button of
    mbLeft : btnIdx := 0;
    mbMiddle : btnIdx := 1;
    mbRight : btnIdx := 2;
  end;
  if FFloor <> nil then
    if FFloor.CurrentRoom <> nil then
      FFloor.CurrentRoom.MouseClick(btnIdx, x, y);
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  AutoInit;
  RenderScene;
end;

procedure TfrmMain.AutoInit;
begin
  if not FMain.Inited3D then
  begin
    FMain.Window := Handle;
    FMain.Init3D(apiDX11);
  end;
  FMain.Projection.DepthRange := Vec(1, 0);
  FMain.Projection.NearPlane := 0.2;
  FMain.Projection.FarPlane := 200;
  FMain.States.DepthFunc := cfGreater;
  FMain.States.ColorMask[0] := AllChanells;
  FMain.UpdateStatesInterval := 8;
end;

procedure TfrmMain.RenderScene;
begin
  MakeNewGameIfNeeded;

  Caption := 'FPS: ' + IntToStr(FFPSLastCount);
  //if FFloor <> nil then
  //  if FFloor.CurrentRoom <> nil then
  //    Caption := IntToStr(FFloor.CurrentRoom.MovedTile.x) + ' ' + IntToStr(FFloor.CurrentRoom.MovedTile.y);

	if FMain.Bind then
  try
    FDefFBO.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
    FDefFBO.Select();

    if not FStartTitle.Visible then
    begin
      FFloor.CurrentRoom.PrepareToDraw();
      FWorld.Renderer.PrepareToDraw;
      FWorld.Renderer.DrawWorld;
  	  FFloor.Draw2DUI();
    end
    else
    begin
      FDefFBO.Clear(0, Vec(0,0,0,0));
      FStartTitle.Draw();
    end;

    FMain.ActiveFrameBuffer.BlitToWindow();

    FMain.Present;

    Inc(FFPSCounter);
    if (FFPSLastSec <> FMain.Time64 div 1000) then
    begin
      FFPSLastSec := FMain.Time64 div 1000;
      FFPSLastCount := FFPSCounter;
      FFPSCounter := 0;
    end;
  finally
    FMain.Unbind;
  end;
end;

procedure TfrmMain.InitWorld;

  procedure PreloadGlyphs;
  const
    cRusGlyphs: string = 'АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдеёжзийклмнопрстуфхцчшщъыьэюя';
    cFont = 'Segoe UI';
  var
    cd: TavCanvasCommonData;
    yyyy: TVec4;
    xxx: TVec3;
    ch: WideChar;
    i: Integer;
    cRusGlyphsWStr: UnicodeString;
  begin
    cRusGlyphsWStr := UnicodeString(cRusGlyphs);

    cd := GetCanvasCommonData(FMain);
    if FileExists('glyphs.cache') then
      cd.LoadCache('glyphs.cache')
    else
    begin
      for ch := Chr(32) to Chr(126) do
        cd.GetGlyphImage(cFont, ch, [], xxx, yyyy);
      for i := 1 to Length(cRusGlyphsWStr) do
        cd.GetGlyphImage(cFont, cRusGlyphsWStr[i], [], xxx, yyyy);

      for ch := Chr(32) to Chr(126) do
        cd.GetGlyphImage(cFont, ch, [gsBold], xxx, yyyy);
      for i := 1 to Length(cRusGlyphsWStr) do
        cd.GetGlyphImage(cFont, cRusGlyphsWStr[i], [gsBold], xxx, yyyy);
      cd.SaveCache('glyphs.cache');
    end;
  end;

  procedure PreloadModels;
  begin
    FWorld.Renderer.PreloadModels([ExeRelativeFileName('models\scene1.avm')]);
    FWorld.Renderer.SetEnviromentCubemap(ExeRelativeFileName('waterfall.dds'));

    FWorld.Renderer.PreloadModels([ExeRelativeFileName('units\archer.avm')]);
    FWorld.Renderer.PreloadModels([ExeRelativeFileName('units\arissa.avm')]);
    FWorld.Renderer.PreloadModels([ExeRelativeFileName('units\hero.avm')]);
    FWorld.Renderer.PreloadModels([ExeRelativeFileName('units\mutant.avm')]);
    FWorld.Renderer.PreloadModels([ExeRelativeFileName('units\wisp.avm')]);
    FWorld.Renderer.PreloadModels([ExeRelativeFileName('bullets\bullets.avm')]);
    FWorld.Renderer.PreloadModels([ExeRelativeFileName('weapons\weapons.avm')]);
  end;

begin
  FWorld := TbWorld.Create(FMain);
  FWorld.Renderer.OnAfterDraw := {$IfDef FPC}@{$EndIf}OnAfterWorldDraw;
  PreloadModels;
  PreloadGlyphs;

  //FWaitForNewGame := True;
  //MakeNewGameIfNeeded;
end;

procedure TfrmMain.OnAfterWorldDraw(Sender: TObject);
begin
  FFloor.CurrentRoom.Draw3DUI();
end;

procedure TfrmMain.MakeNewGameIfNeeded();
begin
  if not FWaitForNewGame then Exit;
  FWaitForNewGame := False;

  FreeAndNil(FFloor);

  FFloor := TFloorMap.Create(FWorld);
  FFloor.CreateLab(7);
  FFloor.UI.OnMenuNewGame := {$IfDef FPC}@{$EndIf}MakeNewGame_Delayed;
  FFloor.UI.OnMenuExit := {$IfDef FPC}@{$EndIf}DoExit_Delayed;
  //FFloor.CreateLab(8);

  if FUPSObj = nil then
    FUPSObj := TUPSObject.Create(FMain);
  FUPSObj.SetState(FWorld, FFloor, FStartTitle);
end;

procedure TfrmMain.MakeNewGame_Delayed(ASender: TObject);
begin
  FWaitForNewGame := True;
  FStartTitle.Visible := False;
end;

procedure TfrmMain.DoExit_Delayed(ASender: TObject);
begin
  FWaitForExit := True;
end;

end.

