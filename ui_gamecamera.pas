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

    procedure UpdateCameraPosition;
  protected
    procedure AfterRegister; override;

    procedure Notify_KeyDown(AKey: Word; const Ex: TKeyEventEx); override;

    procedure Notify_MouseMove(const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts); override;
    procedure Notify_MouseDown(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseUp(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
  public
    property YPlane: Single read FYPlane write FYPlane;
    property RotateSens: Single read FRotateSens write FRotateSens;
    property WheelSens : Single read FWheelSens  write FWheelSens;

    procedure LookAt(const APt: TVec3); overload;
    procedure LookAt(const APt, AViewDir: TVec3); overload;
  end;

implementation

uses
  Math;

{ TavmCameraControl }

procedure TavmCameraControl.UpdateCameraPosition;
var viewDir: TVec3;
begin
  viewDir := Quat(Vec(0,0,-1), FPitch) * Vec(FDist,0,0);
  viewDir := Quat(Vec(0,-1,0), FYaw) * viewDir;
  Main.Camera.Eye := Main.Camera.At + viewDir;
end;

procedure TavmCameraControl.AfterRegister;
begin
  inherited AfterRegister;
  //FYPlane := 1;
  FYPlane := 0;
  FDragPlane := Plane(0, 1, 0, -FYPlane);
  FRotateSens := 0.005;
  FWheelSens := 1/1.15;

  Main.Camera.At := Vec(0,FYPlane,0);

  FYaw := Pi*0.2;
  FPitch := Pi*0.25;
  FDist := 4;
  UpdateCameraPosition;
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
  if sMiddle in AShifts then
  begin
    delta := APt - FLastCoords;
    FLastCoords := APt;

    if sShift in AShifts then
    begin
      if FHoldPtValid then
        if Intersect(FDragPlane, Main.Cursor.Ray, IntPt) then
        begin
          Main.Camera.At := Main.Camera.At - (IntPt - FHoldPt);
          UpdateCameraPosition;
        end;
    end
    else
    begin
      FYaw := FYaw + delta.x * FRotateSens;
      //FPitch := Clamp(FPitch + delta.y * FRotateSens, 0.1*Pi, 0.49*Pi);
      FPitch := Clamp(FPitch + delta.y * FRotateSens, -0.49*Pi, 0.49*Pi);
      UpdateCameraPosition;
    end;
  end;
end;

procedure TavmCameraControl.Notify_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts);
begin
  inherited Notify_MouseWheel(APt, AWheelShift, AShifts);
  FDist := FDist * Power(FWheelSens, AWheelShift);
  FDist := Clamp(FDist, 1, 50);
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

procedure TavmCameraControl.LookAt(const APt: TVec3);
begin
  Main.Camera.At := Vec(APt.x, FYPlane, APt.z);
  FDist := 5;
  UpdateCameraPosition;
end;

procedure TavmCameraControl.LookAt(const APt, AViewDir: TVec3);
begin
  Main.Camera.At := Vec(APt.x, FYPlane, APt.z);
  FYaw  := -arctan2(-AViewDir.z, -AViewDir.x);
  FDist := 5;
  UpdateCameraPosition;
end;

end.

