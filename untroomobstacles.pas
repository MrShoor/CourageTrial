unit untRoomObstacles;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  untLevel, mutils, bWorld, bLights, untObstacles;

type

  { TLightSource }

  TLightSource = class(TObstacle)
  protected
    FLight: IavPointLight;
  protected
    function LightOffset(): TVec3; virtual; abstract;

    procedure SetRoomDir(const AValue: Integer); override;
    procedure SetRoomPos(const AValue: TVec2i); override;
  public
    procedure LoadModels(const AObstacle: TObstacleDesc); override;
    procedure SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean = True); override;
  end;

  { TLantern }

  TLantern = class (TLightSource)
  protected
    function LightOffset(): TVec3; override;
  end;

  { TBrazier }

  TBrazier = class (TLightSource)
  protected
    function LightOffset(): TVec3; override;
  end;

  { TTorchStand }

  TTorchStand = class (TLightSource)
  protected
    function LightOffset(): TVec3; override;
  end;

implementation

{ TTorchStand }

function TTorchStand.LightOffset(): TVec3;
begin
  Result := Vec(0, 1.72562, 0);
end;

{ TBrazier }

function TBrazier.LightOffset(): TVec3;
begin
  Result := Vec(0, 1, 0);
end;

{ TLightSource }

procedure TLightSource.SetRoomDir(const AValue: Integer);
begin
  inherited SetRoomDir(AValue);
  FLight.Pos := LightOffset() * Transform();
end;

procedure TLightSource.SetRoomPos(const AValue: TVec2i);
begin
  inherited SetRoomPos(AValue);
  FLight.Pos := LightOffset() * Transform();
end;

procedure TLightSource.LoadModels(const AObstacle: TObstacleDesc);
begin
  inherited LoadModels(AObstacle);
  FLight := World.Renderer.CreatePointLight();
  FLight.Pos := LightOffset();
  FLight.Radius := 10;
  FLight.Size := 0.1;
  //FLight.Color := Vec(1,1,1.0)*4.41;
  FLight.Color := Pow(Vec(1,0.655,0),1.0)*4.41*4;
  FLight.CastShadows := st512;
end;

procedure TLightSource.SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean);
begin
  inherited SetRoomPosDir(APos, ADir, AAutoRegister);
  FLight.Pos := LightOffset() * Transform();
end;

{ TLantern }

function TLantern.LightOffset(): TVec3;
begin
  Result := Vec(0.89635, 2.51368, 0.03713);
end;

initialization
  RegRoomClass(TLantern);
  RegRoomClass(TBrazier);
  RegRoomClass(TTorchStand);

end.

