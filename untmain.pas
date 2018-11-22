unit untMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, untLevel, avBase, avRes, avTypes, mutils,
  bWorld;

type

  { TUPSObject }

  TUPSObject = class(TavObject)
  private
    FWorld: TbWorld;
    FRoom : TBattleRoom;
  protected
    procedure EMUps(var msg: TavMessage); message EM_UPS;
  public
    procedure SetState(AWorld: TbWorld; ARoom: TBattleRoom);
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
    FRoom: TBattleRoom;

    procedure AutoInit;
    procedure RenderScene;

    procedure InitWorld;
    procedure OnAfterWorldDraw(Sender: TObject);
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TUPSObject }

procedure TUPSObject.EMUps(var msg: TavMessage);
begin
  FWorld.UpdateStep();
  FRoom.UpdateStep();
end;

procedure TUPSObject.SetState(AWorld: TbWorld; ARoom: TBattleRoom);
begin
  FWorld := AWorld;
  FRoom := ARoom;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMain := TavMainRender.Create(nil);
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
  FRoom.KeyPress(Key);
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var btnIdx: Integer;
begin
  case Button of
    mbLeft : btnIdx := 0;
    mbMiddle : btnIdx := 1;
    mbRight : btnIdx := 2;
  end;
  FRoom.MouseClick(btnIdx, x, y);
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
  FMain.Projection.NearPlane := 0.1;
  FMain.Projection.FarPlane := 100;
  FMain.States.DepthFunc := cfGreater;
  FMain.States.ColorMask[0] := AllChanells;
  FMain.UpdateStatesInterval := 8;
end;

procedure TfrmMain.RenderScene;
begin
  if FRoom <> nil then
    Caption := IntToStr(FRoom.MovedTile.x) + ' ' + IntToStr(FRoom.MovedTile.y);

	if FMain.Bind then
  try
    FDefFBO.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
    FDefFBO.Select();

    FRoom.PrepareToDraw();
    FWorld.Renderer.PrepareToDraw;
    FMain.Clear(Black, True, FMain.Projection.DepthRange.y, True);
    FWorld.Renderer.DrawWorld;
  	FRoom.Draw2DUI();

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

  FRoom := TBattleRoom.Create(FWorld);
  FRoom.GenerateWithLoad('rooms\r1.room');

  if FUPSObj = nil then
    FUPSObj := TUPSObject.Create(FMain);
  FUPSObj.SetState(FWorld, FRoom);
end;

procedure TfrmMain.OnAfterWorldDraw(Sender: TObject);
begin
  FRoom.Draw3DUI();
end;

end.

