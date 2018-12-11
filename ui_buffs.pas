unit ui_buffs;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils,
  untLevel,
  avMiniControls, avCanvas, mutils;

type
  { TavmUnitBuffs }

  TavmUnitBuffs = class(TavmCustomControl)
  private const
    cIconSize = 48;
    cIconSpacing = 5;
  private
    FBuffs: IUnitBuffsArr;
    FOldBuffs: IUnitBuffsArr;
    FOldBuffsDuration: array of Integer;
    procedure SetBuffs(const AValue: IUnitBuffsArr);
  protected
    procedure DoValidate; override;
    procedure DrawControl(const AMat: TMat3); override;
  public
    property Buffs: IUnitBuffsArr read FBuffs write SetBuffs;
  end;

implementation

uses Math;

{ TavmUnitBuffs }

procedure TavmUnitBuffs.SetBuffs(const AValue: IUnitBuffsArr);
begin
  if FBuffs = AValue then Exit;
  FBuffs := AValue;
  Invalidate;
end;

procedure TavmUnitBuffs.DoValidate;
var cellpos: TVec2;
    i: Integer;
    tl, br: TVec2;
    txtBuilder: ITextBuilder;
    txtLines  : ITextLines;
begin
  inherited DoValidate;
  Canvas.Clear;
  Canvas.Font.Size := 24;
  Canvas.Font.Color := Vec(1,1,1,1);

  if FOldBuffs = nil then Exit;
  if FOldBuffs.Count = 0 then Exit;

  for i := 0 to FOldBuffs.Count - 1 do
  begin
    Canvas.Brush.Color := Vec(1,1,1,1);
    tl := Vec(i * (cIconSize + cIconSpacing), cIconSpacing);
    br := tl + Vec(cIconSize, cIconSize);
    Canvas.AddSprite(tl, br, 'ui\buffs\' + FOldBuffs[i].Ico);

    txtBuilder := Canvas.TextBuilder;
    txtBuilder.Align := laRight;
    txtBuilder.Write(IntToStr(FOldBuffsDuration[i]));
    txtLines := txtBuilder.Finish();
    txtLines.BoundsX := Vec(tl.x, br.x);
    txtLines.BoundsY := Vec(tl.y, br.y);
    txtLines.VAlign := 1;

    Canvas.Brush.Color := Vec(0,0,0,1);
    Canvas.AddFill(br - Vec(txtLines.MaxLineWidth() + 3, txtLines.TotalHeight()), br + Vec(3, 0));

    Canvas.Brush.Color := Vec(1,1,1,1);
    Canvas.AddText(txtLines);
  end;
end;

procedure TavmUnitBuffs.DrawControl(const AMat: TMat3);

  function IsEqual(const ABuffs1, ABuffs2: IUnitBuffsArr): Boolean;
  var i: Integer;
  begin
    if ABuffs1 = ABuffs2 then Exit(True);
    if ABuffs1 = nil then Exit(False);
    if ABuffs2 = nil then Exit(False);
    if ABuffs1.Count <> ABuffs2.Count then Exit(False);
    for i := 0 to ABuffs1.Count - 1 do
      if ABuffs1[i] <> ABuffs2[i] then Exit(False);
    Result := True;
  end;

  function SyncBuffsDuration: Boolean;
  var i: Integer;
  begin
    Result := False;
    if FOldBuffs = nil then Exit;
    if Length(FOldBuffsDuration) <> FOldBuffs.Count then
    begin
      SetLength(FOldBuffsDuration, FOldBuffs.Count);
      Result := True;
    end;
    for i := 0 to FOldBuffs.Count - 1 do
      if FOldBuffsDuration[i] <> FOldBuffs[i].Duration then
      begin
        Result := True;
        FOldBuffsDuration[i] := FOldBuffs[i].Duration;
      end;
  end;

begin
  if not IsEqual(FOldBuffs, FBuffs) then
  begin
    if FBuffs <> nil then
      FOldBuffs := FBuffs.Clone()
    else
      FOldBuffs := nil;
    Invalidate;
  end;

  if SyncBuffsDuration then
    Invalidate;

  if FOldBuffs <> nil then
    Size := Vec(FOldBuffs.Count * cIconSize + Max(0, FOldBuffs.Count - 1) * cIconSpacing, cIconSize + cIconSpacing)
  else
    Size := Vec(0,0);

  inherited DrawControl(AMat);
end;

end.

