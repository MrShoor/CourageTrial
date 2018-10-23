unit untRoomEditorMain;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Windows,
  LCLType,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls,
  avRes, avTypes, avCameraController,
  mutils,
  untLevel, Types;

const
  cPreviewSize = 96;

type

  { TfmrMain }

  TfmrMain = class(TForm)
    lbObjects: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    Panel1: TPanel;
    Splitter2: TSplitter;
    ToolPanel: TPanel;
    RenderPanel: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbObjectsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure RenderPanelPaint(Sender: TObject);
  private
    FMain: TavMainRender;
    FDefFBO: TavFrameBuffer;

    FRoom: TBattleRoom;

    FObstaclePreviews: array of TBitmap;

    procedure RenderScene;
    procedure CreateNewRoom;
    function ObtainObstaclePreview(const AIndex: Integer): TBitmap;
    procedure CleanObstaclePreview;
  public

  end;

var
  fmrMain: TfmrMain;

implementation

{$R *.lfm}

{ TfmrMain }

procedure TfmrMain.RenderPanelPaint(Sender: TObject);
begin
  RenderScene();
end;

procedure TfmrMain.FormCreate(Sender: TObject);
begin
  lbObjects.ItemHeight := cPreviewSize + 16;

  FMain := TavMainRender.Create(nil);
  FMain.Window := RenderPanel.Handle;
  FMain.Init3D(T3DAPI.apiDX11);
  FMain.Projection.DepthRange := Vec(1, 0);
  FMain.Projection.NearPlane := 0.1;
  FMain.Projection.FarPlane := 100;
  FMain.States.DepthFunc := cfGreater;
  FMain.States.ColorMask[0] := AllChanells;
  FMain.UpdateStatesInterval := 8;

  FDefFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f], [true, false]);

  with TavCameraController.Create(FMain) do
  begin
    MovePlane := Plane(0,1,0,0);
    CanRotate := True;
    CanMove := True;
  end;

  CreateNewRoom();
end;

procedure TfmrMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
  CleanObstaclePreview;
end;

procedure TfmrMain.lbObjectsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var cnv: TCanvas;
    bgColor: TColor;
    previewRect: TRect;
begin
  cnv := (Control as TListBox).Canvas;
  if odSelected in State then
    bgColor := clLtGray
  else
    bgColor := clDkGray;
  cnv.Brush.Color := bgColor;
  cnv.Brush.Style := bsSolid;
  cnv.FillRect(ARect);

  cnv.Pen.Color := clBlack;
  cnv.Line(ARect.Left, ARect.Bottom - 1, ARect.Right, ARect.Bottom - 1);

  if odFocused in State then
    cnv.DrawFocusRect(ARect);

  cnv.Font.Color := clBlack;
  cnv.Font.Size := 14;
  cnv.Brush.Style := bsClear;
  cnv.TextOut(ARect.Left + cPreviewSize + 8*2, ARect.Top + 20, FRoom.Obstacles[Index].name);

  previewRect := Rect(ARect.Left + 8, ARect.Top + 8, ARect.Left + 8+cPreviewSize, ARect.Top + 8+cPreviewSize);
  cnv.Draw(previewRect.Left, previewRect.Top, ObtainObstaclePreview(Index));

  cnv.Rectangle(previewRect);
end;

procedure TfmrMain.RenderScene;
begin
  if FMain.Bind then
  try
    FDefFBO.FrameRect := RectI(0, 0, FMain.WindowSize.x, FMain.WindowSize.y);
    FDefFBO.Select();

    FRoom.Draw();

    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

procedure TfmrMain.CreateNewRoom;
var
  i: Integer;
begin
  FreeAndNil(FRoom);
  FRoom := TBattleRoom.Create(FMain);
  FRoom.GenerateEmpty();

  lbObjects.Clear;
  for i := 0 to FRoom.Obstacles.Count - 1 do
    lbObjects.AddItem(IntToStr(i), nil);

  CleanObstaclePreview;
  SetLength(FObstaclePreviews, FRoom.Obstacles.Count);
end;

function TfmrMain.ObtainObstaclePreview(const AIndex: Integer): TBitmap;
begin
  Result := FObstaclePreviews[AIndex];
  if Result = nil then
  begin
    Result := TBitmap.Create;
    Result.PixelFormat := pf24bit;
    Result.Width := cPreviewSize;
    Result.Height := cPreviewSize;

    if FMain.Bind then
    try
      FRoom.DrawObstaclePreview(FRoom.Obstacles[AIndex].name, Result);
    finally
      FMain.Unbind;
    end;
    FObstaclePreviews[AIndex] := Result;
  end;
end;

procedure TfmrMain.CleanObstaclePreview;
var
  i: Integer;
begin
  for i := 0 to Length(FObstaclePreviews) - 1 do
    FreeAndNil(FObstaclePreviews[i]);
end;

end.

