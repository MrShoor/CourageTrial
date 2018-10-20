unit untEnemies;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Math,
  Classes, SysUtils, avBase, avRes, bWorld, mutils, bLights, avMesh, avTypes, avTess, avContnrs, avContnrsDefaults,
  avPathFinder, untLevel;

type

  { TBot }

  TBot = class (TRoomUnit)
  protected
    procedure AfterRegister; override;
    function FindEnemy(): TRoomUnit; virtual;
  public
    function DoAction(): IBRA_Action;
  end;
  TBotArr = {$IfDef FPC}specialize{$EndIf} TArray<TBot>;
  IBotArr = {$IfDef FPC}specialize{$EndIf} IArray<TBot>;
  TBotSet = {$IfDef FPC}specialize{$EndIf} THashSet<TBot>;
  IBotSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TBot>;


  { TBotMutant1 }

  TBotMutant1 = class (TBot)
  private
    FAnim: IavAnimationController;
  protected
    procedure UpdateStep; override;
  public
    procedure SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean); override;
    function  GetUnitMoveSpeed: Single; override;

    procedure LoadModels(); override;
  end;

implementation

{ TBot }

procedure TBot.AfterRegister;
begin
  inherited AfterRegister;
  SubscribeForUpdateStep;
end;

function TBot.FindEnemy: TRoomUnit;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Room.ChildCount() - 1 do
    if Room.Child[i] is TPlayer then
      Exit(Room.Child[i] as TRoomUnit);
end;

function TBot.DoAction: IBRA_Action;
var player: TRoomUnit;
    path: IRoomPath;
begin
  player := FindEnemy();
  if player = nil then Exit(nil);
  if AP = 0 then Exit(nil);

  if Room.Distance(player.RoomPos, RoomPos) > 1 then
  begin
    path := FindPath(player.RoomPos, player);
    if (path <> nil) and (path.Count > 0) then
    begin
      Result := TBRA_UnitMovementAction.Create(Self, path);
    end;
  end;
end;

{ TBotMutant1 }

procedure TBotMutant1.UpdateStep;
begin
  inherited UpdateStep;
  FAnim.SetTime(World.GameTime);
end;

procedure TBotMutant1.SetAnimation(const ANameSequence: array of string;
  const ALoopedLast: Boolean);
begin
  inherited SetAnimation(ANameSequence, ALoopedLast);
  FAnim.AnimationSequence_StartAndStopOther(ANameSequence, ALoopedLast);
end;

function TBotMutant1.GetUnitMoveSpeed: Single;
begin
  Result := 5;
end;

procedure TBotMutant1.LoadModels;
begin
  AddModel('Mutant1', mtDefault);
	FAnim := Create_IavAnimationController(FModels[0].Mesh.Pose, World.GameTime);
	SetAnimation(['Idle0'], False);

  MaxAP := 8;
  MaxHP := 100;
  HP := MaxHP;

  Preview96_128 := 'ui\units\mutant1.png';
end;

end.

