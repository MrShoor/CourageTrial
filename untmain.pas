unit untMain;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, untLevel, avBase, avRes, avTypes, mutils,
  untFloor,
  bWorld;

type

  { TUPSObject }

  TUPSObject = class(TavObject)
  private
    FFloor: TFloorMap;
    FWorld: TbWorld;
  protected
    procedure EMUps(var msg: TavMessage); message EM_UPS;
  public
    procedure SetState(AWorld: TbWorld; AFloor: TFloorMap);
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
    FMain: TavMainRender;
    FDefFBO: TavFrameBuffer;

    FUPSObj: TUPSObject;

    FWorld: TbWorld;
    FFloor: TFloorMap;

    procedure AutoInit;
    procedure RenderScene;

    procedure InitWorld;
    procedure OnAfterWorldDraw(Sender: TObject);
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
var
  i: Integer;
  rct: TRect;
begin
  if GetForegroundWindow = FWorld.Main.Window then
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

procedure TUPSObject.SetState(AWorld: TbWorld; AFloor: TFloorMap);
begin
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

  InitWorld;
end;

procedure TfrmMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
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
  FFloor.CurrentRoom.KeyPress(Key);
  if Key = Ord(' ') then
    FWorld.Renderer.InvalidateShaders;
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var btnIdx: Integer;
begin
  case Button of
    mbLeft : btnIdx := 0;
    mbMiddle : btnIdx := 1;
    mbRight : btnIdx := 2;
  end;
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
  if FFloor.CurrentRoom <> nil then
    Caption := IntToStr(FFloor.CurrentRoom.MovedTile.x) + ' ' + IntToStr(FFloor.CurrentRoom.MovedTile.y);

	if FMain.Bind then
  try
    FDefFBO.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
    FDefFBO.Select();

    FFloor.CurrentRoom.PrepareToDraw();
    FWorld.Renderer.PrepareToDraw;
    FWorld.Renderer.DrawWorld;
  	FFloor.Draw2DUI();

    FMain.ActiveFrameBuffer.BlitToWindow();

    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

procedure TfrmMain.InitWorld;
  procedure PreloadModels;
  begin
    FWorld.Renderer.PreloadModels([ExeRelativeFileName('models\scene1.avm')]);
    FWorld.Renderer.SetEnviromentCubemap(ExeRelativeFileName('waterfall.dds'));

    FWorld.Renderer.PreloadModels([ExeRelativeFileName('units\units.avm')]);
    FWorld.Renderer.PreloadModels([ExeRelativeFileName('bullets\bullets.avm')]);
    FWorld.Renderer.PreloadModels([ExeRelativeFileName('weapons\weapons.avm')]);
  end;

begin
  FWorld := TbWorld.Create(FMain);
  FWorld.Renderer.OnAfterDraw := {$IfDef FPC}@{$EndIf}OnAfterWorldDraw;
  PreloadModels;

  FFloor := TFloorMap.Create(FWorld);
  FFloor.Create2Rooms;
  //FFloor.CreateLab(8);

  if FUPSObj = nil then
    FUPSObj := TUPSObject.Create(FMain);
  FUPSObj.SetState(FWorld, FFloor);
end;

procedure TfrmMain.OnAfterWorldDraw(Sender: TObject);
begin
  FFloor.CurrentRoom.Draw3DUI();
end;

end.

