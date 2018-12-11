unit ui_enemies;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils,
  avBase, avRes, avMiniControls, avContnrs, avCanvas, untLevel, mutils, avTypes, intfUtils;

type

  { TavmEnemiesBar }

  TavmEnemiesBar = class (TavmCustomControl)
  private const
    cBorderSize = 5;
    cPicSizeX = 96 div 2;
    cPicSizeY = 128 div 2;
  private type
    IRoomUnitWeakArr = {$IfDef FPC}specialize{$EndIf}IArray<IWeakRef>;
    TRoomUnitWeakArr = {$IfDef FPC}specialize{$EndIf}TArray<IWeakRef>;
  private
    FEnemies: IRoomUnitWeakArr;
    FHighlightedIdx: Integer;
    function IndexOf(const AEnemy: TRoomUnit): Integer;
    function ItemRect(const AEnemyIndex: Integer): TRectF;
    procedure AdjustSize;
  protected
    procedure AfterRegister; override;
    procedure DrawControl(const AMat: TMat3); override;
    procedure DoValidate; override;
  public
    procedure SetEnemies(const AValue: IRoomUnitArr);
    procedure SetHighlighedEnemy(const AEnemy: TRoomUnit);

    function EnemiesCount: Integer;
    function Enemy(AIndex: Integer): TRoomUnit;
  end;

implementation

{ TavmEnemiesBar }

function TavmEnemiesBar.IndexOf(const AEnemy: TRoomUnit): Integer;
var
  i: Integer;
begin
  for i := 0 to EnemiesCount - 1 do
    if Enemy(i) = AEnemy then Exit(i);
  Result := -1;
end;

function TavmEnemiesBar.ItemRect(const AEnemyIndex: Integer): TRectF;
begin
  Result.min.x := AEnemyIndex * (cPicSizeX + cBorderSize) + cBorderSize;
  Result.min.y := cBorderSize;
  Result.max := Result.min + Vec(cPicSizeX, cPicSizeY);
end;

procedure TavmEnemiesBar.AdjustSize;
begin
  Size := Vec(EnemiesCount * (cPicSizeX + cBorderSize) + cBorderSize, cPicSizeY + cBorderSize * 2);
end;

procedure TavmEnemiesBar.AfterRegister;
begin
  inherited AfterRegister;
  Origin := Vec(0.5, 0);
  FHighlightedIdx := -1;
end;

procedure TavmEnemiesBar.DrawControl(const AMat: TMat3);
begin
  Pos := Vec(Main.WindowSize.x * 0.5, 0);
  inherited DrawControl(AMat);
end;

procedure TavmEnemiesBar.DoValidate;
var i: Integer;
    unt: TRoomUnit;
    rct: TRectF;
begin
  inherited DoValidate;
  Canvas.Clear;

  for i := 0 to EnemiesCount - 1 do
  begin
    unt := Enemy(i);
    rct := ItemRect(i);

    if (unt = nil) or (unt.Preview96_128 = '') then
    begin
      Canvas.Brush.Color := Vec(0.125, 0.125, 0.125, 1);
      Canvas.AddFill(rct.min, rct.max);
    end
    else
    begin
      Canvas.Brush.Color := Vec(1,1,1,1);
      Canvas.AddSprite(rct.min, rct.max, unt.Preview96_128);
      if unt.IsDead() then
      begin
        Canvas.Brush.Color := Vec(1,1,1,0.5);
        Canvas.AddSprite(rct.min, rct.max, 'ui\units\skull.png');
      end;
    end;
  end;

  for i := 0 to EnemiesCount - 1 do
  begin
    rct := ItemRect(i);
    if i = FHighlightedIdx then
    begin
      Canvas.Pen.Color := Vec(1, 215/255, 0, 1);
      Canvas.Pen.Width := 3;
    end
    else
    begin
      Canvas.Pen.Color := Vec(0, 0, 0, 1);
      Canvas.Pen.Width := 1;
    end;
    Canvas.AddRectangle(rct.min, rct.max);
  end;
end;

procedure TavmEnemiesBar.SetEnemies(const AValue: IRoomUnitArr);
var i: Integer;
begin
  FEnemies := TRoomUnitWeakArr.Create();
  if AValue = nil then
    Exit;

  for i := 0 to AValue.Count - 1 do
    FEnemies.Add(AValue[i].WeakRef);

  AdjustSize;
  Invalidate;
end;

procedure TavmEnemiesBar.SetHighlighedEnemy(const AEnemy: TRoomUnit);
begin
  FHighlightedIdx := IndexOf(AEnemy);
  Invalidate;
end;

function TavmEnemiesBar.EnemiesCount: Integer;
begin
  if FEnemies = nil then Exit(0);
  Result := FEnemies.Count;
end;

function TavmEnemiesBar.Enemy(AIndex: Integer): TRoomUnit;
begin
  Result := nil;
  if FEnemies = nil then Exit;
  if AIndex < 0 then Exit;
  if AIndex >= FEnemies.Count then Exit;
  Result := TRoomUnit(FEnemies[AIndex].Obj);
end;

end.

