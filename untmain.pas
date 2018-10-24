unit untMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, untLevel, avRes, avTypes, mutils, avCameraController;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private
    FMain: TavMainRender;
    FDefFBO: TavFrameBuffer;

    FRoom: TBattleRoom;
    procedure AutoInit;
    procedure RenderScene;

    procedure InitWorld;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMain := TavMainRender.Create(nil);
  FDefFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f], [True, False]);

  with TavCameraController.Create(FMain) do
  begin
    MovePlane := Plane(0,1,0,0);
    CanRotate := True;
    CanMove := True;
  end;

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
	if FMain.Bind then
  try
    FDefFBO.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
    FDefFBO.Select();

  	FRoom.Draw();
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

procedure TfrmMain.InitWorld;
begin
  FRoom := TBattleRoom.Create(FMain);
  FRoom.GenerateWithLoad('rooms\r1.room');
end;

end.

