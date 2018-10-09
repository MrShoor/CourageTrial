unit untLevel;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avBase, avRes, bWorld, mutils, bLights, avMesh, avTypes;

type
  { TLantern }

  TLantern = class (TbGameObject)
  private
  	FLight: IavPointLight;
  public
    procedure LoadModels();
  end;

  { TPlayer }

  TPlayer = class (TbGameObject)
  private
  	FAnim: array of IavAnimationController;
  protected
    procedure UpdateStep; override;
  public
    procedure LoadModels();
  end;

  { TRoomMap }

  TRoomMap = class ()
  private
    FRaduis: Integer;
  public
    function Distance(const APt1, APt2: TVec2i): Integer;
    function IsCellExists(const APos: TVec2): Boolean;
    constructor Create(const ARadius: Integer);
  end;

  { TBattleRoom }

  TBattleRoom = class (TavMainRenderChild)
  private
    FWorld: TbWorld;

    FFloor: TbGameObject;
    FLanterns: IbGameObjArr;

    FPlayer: TPlayer;
  protected
    procedure EMUps(var msg: TavMessage); message EM_UPS;
  	procedure AfterRegister; override;
  public
    procedure Draw();
    procedure Generate();
  end;

implementation

{ TRoomMap }

function TRoomMap.Distance(const APt1, APt2: TVec2i): Integer;
begin

end;

function TRoomMap.IsCellExists(const APos: TVec2): Boolean;
begin
  Result := Distance(Vec(0,0), APos) <= FRaduis;
end;

constructor TRoomMap.Create(const ARadius: Integer);
begin
  FRaduis := ARadius;
end;

{ TPlayer }

procedure TPlayer.UpdateStep;
var
  i: Integer;
begin
  inherited UpdateStep;

  for i := 0 to Length(FAnim) - 1 do
	  FAnim[i].SetTime(World.GameTime);
end;

procedure TPlayer.LoadModels();
var
  i: Integer;
begin
  AddModel('Gop_Body', mtDefault);
  AddModel('Gop_Bottoms', mtDefault);
  AddModel('Gop_Hair', mtDefault);
  AddModel('Gop_Hats', mtDefault);
  AddModel('Gop_Shoes', mtDefault);
  AddModel('Gop_Tops', mtDefault);

  SetLength(FAnim, FModels.Count);
  for i := 0 to FModels.Count - 1 do
  begin
	  FAnim[i] := Create_IavAnimationController(FModels[i].Mesh.Pose, World.GameTime);
	  FAnim[i].AnimationStart('Idle0');
  end;
  SubscribeForUpdateStep;
end;

{ TLantern }

procedure TLantern.LoadModels();
begin
  AddModel('Lantern', mtDefault);
  FLight := World.Renderer.CreatePointLight();
  FLight.Pos := Vec(10,10,0);
  FLight.Radius := 130;
  FLight.Color := Vec(1,1,1);
  FLight.CastShadows := True;
end;

{ TBattleRoom }

procedure TBattleRoom.EMUps(var msg: TavMessage);
begin
  FWorld.UpdateStep();
end;

procedure TBattleRoom.AfterRegister;
begin
  inherited AfterRegister;
  FWorld := TbWorld.Create(Self);

  Main.Camera.At := Vec(0,0,0);
  Main.Camera.Up := Vec(0,1,0);
  Main.Camera.Eye := Main.Camera.At + Vec(10, 10, 5);
end;

procedure TBattleRoom.Draw();
begin
  FWorld.Renderer.PrepareToDraw;
  Main.Clear(Black, True, Main.Projection.DepthRange.y, True);
  FWorld.Renderer.DrawWorld;

  Main.ActiveFrameBuffer.BlitToWindow();
end;

procedure TBattleRoom.Generate();
var lantern: TLantern;
begin
  FLanterns := TbGameObjArr.Create();

  FWorld.Renderer.PreloadModels(['models\scene1.avm']);
  FWorld.Renderer.PreloadModels(['chars\gop.avm']);

  FFloor := TbGameObject.Create(FWorld);
  FFloor.AddModel('Floor', mtDefault);

  lantern := TLantern.Create(FWorld);
  lantern.LoadModels();
  FLanterns.Add(lantern);

  FPlayer := TPlayer.Create(FWorld);
  FPlayer.LoadModels();
  FPlayer.Pos := Vec(5,0,5);
end;

end.

