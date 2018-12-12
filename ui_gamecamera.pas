unit ui_gamecamera;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, mutils, avTypes;

type

  { TavmCameraControl }

  TavmCameraControl = class(TavmCustomControl)
  private
    FRotateSens: Single;
    FWheelSens: Single;

    FDragPlane: TPlane;
    FYaw  : Single;
    FPitch: Single;
    FDist : Single;

    FLastCoords: TVec2;
    FHoldPtValid: Boolean;
    FHoldPt: TVec3;
    FYPlane: Single;

    FFloorBoundingRect: TRectF;

    FSmoothPlaying: Boolean;
    FTargetPos    : TVec3;
    FTargetYaw    : Single;
    FTargetPitch  : Single;
    FTargetDist   : Single;

    procedure StopPlaying(const AAtEnd: Boolean = False);
    procedure PlayAt(const ATargetPos: TVec3; ATargetYaw, ATargetPitch, ATargetDist: Single);

    procedure SetFloorBoundingRect(const AValue: TRectF);
    procedure UpdateCameraPosition;
  protected
    procedure AfterRegister; override;

    procedure Notify_KeyDown(AKey: Word; const Ex: TKeyEventEx); override;

    procedure Notify_MouseMove(const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts); override;
    procedure Notify_MouseDown(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseUp(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;

    procedure OnUPS; override;
  public
    property YPlane: Single read FYPlane write FYPlane;
    property RotateSens: Single read FRotateSens write FRotateSens;
    property WheelSens : Single read FWheelSens  write FWheelSens;

    property FloorBoundingRect: TRectF read FFloorBoundingRect write SetFloorBoundingRect;

    procedure LookAt(const APt: TVec3; ASmooth: Boolean = False); overload;
    procedure LookAt(const APt, AViewDir: TVec3); overload;
  end;

implementation

uses
  Math, Windows, avPlatform;

{ TavmCameraControl }

procedure TavmCameraControl.UpdateCameraPosition;
var viewDir: TVec3;
    pt: TVec2;
begin
  if not FFloorBoundingRect.IsEmpty then
  begin
    pt.x := Clamp(Main.Camera.At.x, FFloorBoundingRect.min.x, FFloorBoundingRect.max.x);
    pt.y := Clamp(Main.Camera.At.z, FFloorBoundingRect.min.y, FFloorBoundingRect.max.y);
    Main.Camera.At := Vec(pt.x, FYPlane, pt.y);
  end;

  viewDir := Quat(Vec(0,0,-1), FPitch) * Vec(FDist,0,0);
  viewDir := Quat(Vec(0,-1,0), FYaw) * viewDir;
  Main.Camera.Eye := Main.Camera.At + viewDir;
end;

procedure TavmCameraControl.StopPlaying(const AAtEnd: Boolean);
begin
  if not FSmoothPlaying then Exit;
  FSmoothPlaying := False;

  if AAtEnd then
  begin
    FDist := FTargetDist;
    FYaw := FTargetYaw;
    FPitch := FTargetPitch;
    Main.Camera.At := FTargetPos;
    UpdateCameraPosition;
  end;
end;

procedure TavmCameraControl.PlayAt(const ATargetPos: TVec3; ATargetYaw, ATargetPitch, ATargetDist: Single);
begin
  FTargetDist := ATargetDist;
  FTargetPos := ATargetPos;
  FTargetPitch := ATargetPitch;
  FTargetYaw := ATargetYaw;
  FSmoothPlaying := True;
end;

procedure TavmCameraControl.SetFloorBoundingRect(const AValue: TRectF);
begin
  if FFloorBoundingRect = AValue then Exit;
  FFloorBoundingRect := AValue;
end;

procedure TavmCameraControl.AfterRegister;
begin
  inherited AfterRegister;
  //FYPlane := 1;
  FYPlane := 0.7;
  FDragPlane := Plane(0, 1, 0, -FYPlane);
  FRotateSens := 0.005;
  FWheelSens := 1/1.15;

  Main.Camera.At := Vec(0,FYPlane,0);

  FYaw := Pi*0.2;
  FPitch := Pi*0.25;
  FDist := 4;
  UpdateCameraPosition;
  UPSSubscribe;
end;

procedure TavmCameraControl.Notify_KeyDown(AKey: Word; const Ex: TKeyEventEx);
begin
  inherited Notify_KeyDown(AKey, Ex);
  if sShift in Ex.shifts then
    FHoldPtValid := Intersect(FDragPlane, Main.Cursor.Ray, FHoldPt);
end;

procedure TavmCameraControl.Notify_MouseMove(const APt: TVec2; AShifts: TShifts);
var delta: TVec2;
    IntPt: TVec3;
begin
  inherited Notify_MouseMove(APt, AShifts);
  if sRight in AShifts then
  begin
    delta := APt - FLastCoords;
    FLastCoords := APt;

    if sShift in AShifts then
    begin
      if FHoldPtValid then
        if Intersect(FDragPlane, Main.Cursor.Ray, IntPt) then
        begin
          Main.Camera.At := Main.Camera.At - (IntPt - FHoldPt);
          StopPlaying;
          UpdateCameraPosition;
        end;
    end
    else
    begin
      FYaw := FYaw + delta.x * FRotateSens;
      FPitch := Clamp(FPitch + delta.y * FRotateSens, 0.1*Pi, 0.49*Pi);
      //FPitch := Clamp(FPitch + delta.y * FRotateSens, -0.49*Pi, 0.49*Pi);
      StopPlaying;
      UpdateCameraPosition;
    end;
  end;
end;

procedure TavmCameraControl.Notify_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts);
begin
  inherited Notify_MouseWheel(APt, AWheelShift, AShifts);
  FDist := FDist * Power(FWheelSens, AWheelShift);
  FDist := Clamp(FDist, 1, 150);
  StopPlaying;
  UpdateCameraPosition;
end;

procedure TavmCameraControl.Notify_MouseDown(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_MouseDown(ABtn, APt, AShifts);
  FLastCoords := APt;
  if sShift in AShifts then
    FHoldPtValid := Intersect(FDragPlane, Main.Cursor.Ray, FHoldPt);
end;

procedure TavmCameraControl.Notify_MouseUp(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_MouseUp(ABtn, APt, AShifts);
end;

procedure TavmCameraControl.OnUPS;
const cSpeed = 0.07;
      cBorderWidth = 10;
      cPanSpeed = 0.3;
var cpt: TVec2;
    panDir: TVec2;
    viewDir3D: TVec3;
    viewDir2D: TVec3;
    panOffset: TVec3;
begin
  cpt := GetCursorPos(Main.Window, True, False);

  if (cpt.x >= 0) and
     (cpt.y >= 0) and
     (cpt.x < Main.WindowSize.x) and
     (cpt.y < Main.WindowSize.y) and
     (GetForegroundWindow = Main.Window) then
  begin
    panDir := Vec(0, 0);
    if cpt.x < cBorderWidth then
      panDir.x := panDir.x + 1.0;
    if Main.WindowSize.x - cpt.x < cBorderWidth then
      panDir.x := panDir.x - 1.0;
    if cpt.y < cBorderWidth then
      panDir.y := panDir.y - 1.0;
    if Main.WindowSize.y - cpt.y < cBorderWidth then
      panDir.y := panDir.y + 1.0;

    if LenSqr(panDir) > 0.001 then
    begin
      StopPlaying();

      viewDir3D := Main.Camera.Eye - Main.Camera.At;
      viewDir2D := normalize(Vec(viewDir3D.x, 0, viewDir3D.z));
      panOffset := viewDir2D * panDir.y * cPanSpeed;
      panOffset := panOffset + (viewDir2D * Quat(Vec(0, 1, 0), PI * 0.5)) * panDir.x * cPanSpeed;
      Main.Camera.Eye := Main.Camera.Eye + panOffset;
      Main.Camera.At  := Main.Camera.At + panOffset;
    end;
  end;

  if FSmoothPlaying then
  begin
    FDist := Lerp(FDist, FTargetDist, cSpeed);
    FYaw := Lerp(FYaw, FTargetYaw, cSpeed);
    FPitch := Lerp(FPitch, FTargetPitch, cSpeed);
    Main.Camera.At := Lerp(Main.Camera.At, FTargetPos, cSpeed);
    if (abs(FDist - FTargetDist) < 0.001) and
       (abs(FYaw - FTargetYaw) < 0.005) and
       (abs(FPitch - FTargetPitch) < 0.005) and
       (LenSqr(Main.Camera.At - FTargetPos) < 0.0001) then
    begin
      StopPlaying(True);
    end
    else
      UpdateCameraPosition;
  end;
end;

procedure TavmCameraControl.LookAt(const APt: TVec3; ASmooth: Boolean);
begin
  StopPlaying;
  if ASmooth then
  begin
    PlayAt(Vec(APt.x, FYPlane, APt.z), FYaw, FPitch, 7.5);
  end
  else
  begin
    Main.Camera.At := Vec(APt.x, FYPlane, APt.z);
    FDist := 5;
    UpdateCameraPosition;
  end;
end;

procedure TavmCameraControl.LookAt(const APt, AViewDir: TVec3);
begin
  StopPlaying;
  Main.Camera.At := Vec(APt.x, FYPlane, APt.z);
  FYaw  := -arctan2(-AViewDir.z, -AViewDir.x);
  FDist := 7.5;
  UpdateCameraPosition;
end;

end.

