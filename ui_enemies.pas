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
    procedure GetUnitHP(const AEnemyIndex: Integer; out ACurrentHP, AMaxHP: Integer);
    function HPBarRect(const AEnemyIndex: Integer; const ACurrentHpOnly: Boolean): TRectF;
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

uses Math;

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

procedure TavmEnemiesBar.GetUnitHP(const AEnemyIndex: Integer; out ACurrentHP,
  AMaxHP: Integer);
var
  unt: TRoomUnit;
begin
  unt := Enemy(AEnemyIndex);
  if unt = nil then
  begin
    ACurrentHP := 0;
    AMaxHP := 0;
  end
  else
  begin
    ACurrentHP := Clamp(unt.HP, 0, unt.MaxHP);
    AMaxHP := unt.MaxHP;
  end;
end;

function TavmEnemiesBar.HPBarRect(const AEnemyIndex: Integer; const ACurrentHpOnly: Boolean): TRectF;
var hp, hpmax: Integer;
begin
  Result := ItemRect(AEnemyIndex);
  Result.Top := Result.Bottom + cBorderSize;
  Result.Bottom := Result.Top + 16;
  if ACurrentHpOnly then
  begin
    GetUnitHP(AEnemyIndex, hp, hpmax);
    Result.Right := Lerp(Result.Left, Result.Right, hp / max(1, hpmax));
  end;
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
    hp, hpmax: Integer;
    tb: ITextBuilder;
    tl: ITextLines;
begin
  inherited DoValidate;
  Canvas.Clear;

  //portraits
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

  //hp bars
  for i := 0 to EnemiesCount - 1 do
  begin
    Canvas.Brush.Color := Vec(0.125, 0.125, 0.125, 1);
    rct := HPBarRect(i, False);
    Canvas.AddFill(rct.min, rct.max);

    Canvas.Brush.Color := Vec(1, 0, 0, 1);
    rct := HPBarRect(i, True);
    Canvas.AddFill(rct.min, rct.max);
  end;

  //hp bar text
  Canvas.Font.Size := 16;
  for i := 0 to EnemiesCount - 1 do
  begin
    rct := HPBarRect(i, False);
    GetUnitHP(i, hp, hpmax);
    if hpmax > 0 then
    begin
      tb := Canvas.TextBuilder;
      tb.Align := laCenter;
      tb.Write(IntToStr(hp));
      tb.Write('/');
      tb.Write(IntToStr(hpmax));
      tl := tb.Finish();
      tl.BoundsX := Vec(rct.min.x, rct.max.x);
      tl.BoundsY := Vec(rct.min.y, rct.max.y);
      tl.VAlign := 0.5;
      Canvas.AddText(tl);
    end;
  end;

  //hp bar outlines
  Canvas.Pen.Color := Vec(0, 0, 0, 1);
  Canvas.Pen.Width := 1;
  for i := 0 to EnemiesCount - 1 do
  begin
    rct := HPBarRect(i, False);
    Canvas.AddRectangle(rct.min, rct.max);
  end;

  //portraits outlines
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

