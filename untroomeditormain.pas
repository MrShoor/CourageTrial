unit untRoomEditorMain;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$Else}
  {$Define DCC}
{$EndIf}

interface

uses
  Windows,
  {$IfDef FPC}
  LCLType,
  FileUtil,
  {$EndIf}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls,
  avRes, avTypes, avCameraController, bWorld,
  mutils,
  untWayPoint,
  untLevel, untObstacles, untInteractiveObjects;

const
  cPreviewSize = 96;

type
  TEditActionState = (easNone, easAddObject);

  { TAddObjectState }

  TAddObjectState = record
    obstacleDesc: TObstacleDesc;
    obstacle: TObstacle;
    procedure SetStateNew(const ARoom: TBattleRoom; const AObstacleIndex: Integer); overload;
    procedure SetStateNew(const ARoom: TBattleRoom; const AObstacle: TObstacleDesc); overload;
    procedure SetState(const AObstacle: TObstacle); overload;
    procedure UpdateState(const ARoomPos: TVec2i; const ARoomDir: Integer);
    procedure ClearState();
    procedure CancelState();
  end;

  {$IFnDef FPC}
  TPanel = class(ExtCtrls.TPanel)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
  public
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;
  {$EndIf}

  { TfmrMain }

  TfmrMain = class(TForm)
    lbObjects: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    OpenRoomDialog: TOpenDialog;
    Panel1: TPanel;
    SaveRoomDialog: TSaveDialog;
    Splitter2: TSplitter;
    ToolPanel: TPanel;
    RenderPanel: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbObjectsDblClick(Sender: TObject);
    procedure lbObjectsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure miNewClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure RenderPanelDblClick(Sender: TObject);
    procedure RenderPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure RenderPanelPaint(Sender: TObject);
  private
    FMain: TavMainRender;
    FDefFBO: TavFrameBuffer;

    FWorld: TbWorld;
    FRoom : TBattleRoom;

    FObstaclePreviews: array of TBitmap;

    FState: TEditActionState;
    FState_AddObject: TAddObjectState;

    procedure ClearEditState;

    procedure RenderScene;
    procedure CreateNewRoom;
    function ObtainObstaclePreview(const AIndex: Integer): TBitmap;
    procedure CleanObstaclePreview;
  public

  end;

var
  fmrMain: TfmrMain;

implementation

{$IfnDef DCC}
  {$R *.lfm}
{$Else}
  {$R *.dfm}
{$EndIf}

{ TAddObjectState }

procedure TAddObjectState.SetStateNew(const ARoom: TBattleRoom; const AObstacleIndex: Integer);
begin
  SetStateNew(ARoom, ARoom.Obstacles[AObstacleIndex]);
end;

procedure TAddObjectState.SetStateNew(const ARoom: TBattleRoom; const AObstacle: TObstacleDesc);
var cls: TRoomObjectClass;
begin
  Assert(obstacle = nil);
  obstacleDesc := AObstacle;
  cls := FindRoomClass(obstacleDesc.clsname);
  if cls = nil then
    cls := TObstacle;
  obstacle := ARoom.CreateRoomObject(cls) as TObstacle;
  obstacle.LoadModels(obstacleDesc);
end;

procedure TAddObjectState.SetState(const AObstacle: TObstacle);
begin
  obstacle := AObstacle;
  obstacleDesc := obstacle.Obstacle;
end;

procedure TAddObjectState.UpdateState(const ARoomPos: TVec2i; const ARoomDir: Integer);
begin
  if (obstacle.RoomPos = ARoomPos) and (obstacle.RoomDir = ARoomDir) then Exit;
  obstacle.UnregisterAtRoom();
  if CanPlaceObstacle(obstacle.Room, obstacleDesc, ARoomPos, ARoomDir) then
  begin
    obstacle.RoomPos := ARoomPos;
    obstacle.RoomDir := ARoomDir;
  end;
  if CanPlaceObstacle(obstacle.Room, obstacleDesc, obstacle.RoomPos, obstacle.RoomDir) then
    obstacle.RegisterAtRoom();
end;

procedure TAddObjectState.ClearState;
begin
  obstacle := nil;
end;

procedure TAddObjectState.CancelState;
begin
  FreeAndNil(obstacle);
end;

{ TfmrMain }

procedure TfmrMain.RenderPanelPaint(Sender: TObject);
begin
  RenderScene();
end;

procedure TfmrMain.ClearEditState;
begin
  case FState of
    easAddObject: begin
      FState_AddObject.CancelState();
      FState := easNone;
    end;
  end;
end;

procedure TfmrMain.FormCreate(Sender: TObject);
begin
  RenderPanel.OnPaint := {$IfDef FPC}@{$EndIf}RenderPanelPaint;

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
  FMain.Camera.At := Vec(0, 0, 0);
  FMain.Camera.Up := Vec(0, 1, 0);
  FMain.Camera.Eye := Vec(-10, 20, -10);

  FWorld := TbWorld.Create(FMain);
  FWorld.Renderer.SetEnviromentCubemap(ExeRelativeFileName('waterfall.dds'));
  FWorld.Renderer.PreloadModels([ExeRelativeFileName('models\scene1.avm')]);
  //FWorld.Renderer.PreloadModels([ExeRelativeFileName('units\units.avm')]);
  //FWorld.Renderer.PreloadModels([ExeRelativeFileName('bullets\bullets.avm')]);
  FWorld.Renderer.PreloadModels([ExeRelativeFileName('weapons\weapons.avm')]);

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

procedure TfmrMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case FState of
    easAddObject : begin
      if Key = VK_SPACE then
        FState_AddObject.UpdateState(FState_AddObject.obstacle.RoomPos, (FState_AddObject.obstacle.RoomDir + 1) mod 6);
      if (Key = VK_ESCAPE) or (Key = VK_DELETE) then
      begin
        FState := easNone;
        FState_AddObject.CancelState();
      end;
    end;
  end;

  if Key = VK_SPACE then
  begin
    FRoom.KeyPress(Key);
    FMain.InvalidateWindow;
  end;
end;

procedure TfmrMain.lbObjectsDblClick(Sender: TObject);
begin
  if lbObjects.ItemIndex = -1 then Exit;
  ClearEditState;
  FState_AddObject.SetStateNew(FRoom, lbObjects.ItemIndex);
  FState := easAddObject;
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
  cnv.MoveTo(ARect.Left, ARect.Bottom - 1);
  cnv.LineTo(ARect.Right, ARect.Bottom - 1);

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

procedure TfmrMain.miNewClick(Sender: TObject);
begin
  CreateNewRoom;
end;

procedure TfmrMain.miOpenClick(Sender: TObject);
var fs: TFileStream;
begin
  if OpenRoomDialog.Execute then
  begin
    CreateNewRoom;
    fs := TFileStream.Create(OpenRoomDialog.FileName, fmOpenRead);
    try
      FRoom.LoadRoomMap(fs);
    finally
      FreeAndNil(fs);
    end;
  end;
end;

procedure TfmrMain.miSaveClick(Sender: TObject);
var fs: TFileStream;
begin
  if SaveRoomDialog.Execute then
  begin
    ClearEditState;
    fs := TFileStream.Create(SaveRoomDialog.FileName, fmCreate);
    try
      FRoom.SaveRoomMap(fs);
    finally
      FreeAndNil(fs);
    end;
  end;
end;

procedure TfmrMain.RenderPanelDblClick(Sender: TObject);
var movedTile: TVec2i;
    obj: TRoomObject;
begin
  case FState of
    easNone : begin
      movedTile := FRoom.Map.UI.GetTileAtCoords(FMain.Cursor.Ray);
      obj := FRoom.Map.ObjectAt(movedTile);
      if obj is TObstacle then
      begin
        FState := easAddObject;
        if ssShift in {$IfDef FPC}GetKeyShiftState{$Else}KeyboardStateToShiftState{$EndIf} then
        begin
          FState_AddObject.SetStateNew(FRoom, TObstacle(obj).Obstacle);
          FState_AddObject.obstacle.RoomPos := obj.RoomPos;
          FState_AddObject.obstacle.RoomDir := obj.RoomDir;
        end
        else
          FState_AddObject.SetState(obj as TObstacle);
      end;
    end;
    easAddObject: begin
      FState := easNone;
      if FState_AddObject.obstacle.Registred then
        FState_AddObject.ClearState()
      else
        FState_AddObject.CancelState();
    end;
  end;
end;

procedure TfmrMain.RenderPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var movedTile: TVec2i;
begin
  if FState = easNone then Exit;
  if FState = easAddObject then
  begin
    movedTile := FRoom.Map.UI.GetTileAtCoords(FMain.Cursor.Ray);
    FState_AddObject.UpdateState(movedTile, FState_AddObject.obstacle.RoomDir);
  end;
end;

procedure TfmrMain.RenderScene;
begin
  if FMain.Bind then
  try
    FDefFBO.FrameRect := RectI(0, 0, FMain.WindowSize.x, FMain.WindowSize.y);
    FDefFBO.Select();

    if FRoom = nil then
      FMain.Clear(Vec(0,0,0,0))
    else
    begin
      FRoom.PrepareToDraw();
      FWorld.Renderer.PrepareToDraw;
      FMain.Clear(Black, True, FMain.Projection.DepthRange.y, True);
      FWorld.Renderer.DrawWorld;
    end;

    FMain.ActiveFrameBuffer.BlitToWindow();

    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

procedure TfmrMain.CreateNewRoom;
var
  i: Integer;
begin
  ClearEditState;

  FreeAndNil(FRoom);
  if FMain.Inited3D then
  begin
    FMain.InvalidateWindow;
    UpdateWindow(FMain.Window);
  end;
  FRoom := TBattleRoom.Create(FWorld);
  FRoom.GenerateEmpty();
  FRoom.SetEditMode();

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

{$IFnDef FPC}
{ TPanel }

procedure TPanel.Paint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self)
  else
    inherited;
end;
{$EndIf}

end.

