object frmGraph: TfrmGraph
  Left = 0
  Top = 0
  Caption = 'Graph Layout'
  ClientHeight = 390
  ClientWidth = 650
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object controlPanel: TPanel
    Left = 0
    Top = 348
    Width = 650
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 436
    object btnScramble: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Scramble'
      TabOrder = 0
      OnClick = btnScrambleClick
    end
    object btnShake: TButton
      Left = 89
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Shake'
      TabOrder = 1
      OnClick = btnScrambleClick
    end
    object chkStress: TCheckBox
      Left = 184
      Top = 12
      Width = 59
      Height = 17
      Caption = 'Stress'
      TabOrder = 2
      OnClick = chkStressClick
    end
    object chkRandom: TCheckBox
      Left = 249
      Top = 13
      Width = 72
      Height = 17
      Caption = 'Random'
      TabOrder = 3
      OnClick = chkStressClick
    end
  end
  object edgesPanel: TPanel
    Left = 0
    Top = 299
    Width = 650
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 387
    DesignSize = (
      650
      49)
    object lblEdges: TLabel
      Left = 8
      Top = 8
      Width = 29
      Height = 13
      Caption = 'Edges'
    end
    object lblCenter: TLabel
      Left = 529
      Top = 8
      Width = 33
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Center'
    end
    object edtCenter: TEdit
      Left = 529
      Top = 27
      Width = 113
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnChange = edtEdgesChange
    end
    object edtEdges: TComboBox
      Left = 8
      Top = 27
      Width = 511
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edtEdgesChange
      OnCloseUp = edtEdgesChange
      Items.Strings = (
        
          'joe-food,joe-dog,joe-tea,joe-cat,joe-table,table-plate/50,plate-' +
          'food/30,food-mouse/100,food-dog/100,mouse-cat/150,table-cup/30,c' +
          'up-tea/30,dog-cat/80,cup-spoon/50,plate-fork,dog-flea1,dog-flea2' +
          ',flea1-flea2/20,plate-knife'
        
          'zero-one,one-two,two-three,three-four,four-five,five-six,six-sev' +
          'en,seven-zero'
        
          'zero-one,zero-two,zero-three,zero-four,zero-five,zero-six,zero-s' +
          'even,zero-eight,zero-nine,one-ten,two-twenty,three-thirty,four-f' +
          'ourty,five-fifty,six-sixty,seven-seventy,eight-eighty,nine-ninet' +
          'y,ten-twenty/80,twenty-thirty/80,thirty-fourty/80,fourty-fifty/8' +
          '0,fifty-sixty/80,sixty-seventy/80,seventy-eighty/80,eighty-ninet' +
          'y/80,ninety-ten/80,one-two/30,two-three/30,three-four/30,four-fi' +
          've/30,five-six/30,six-seven/30,seven-eight/30,eight-nine/30,nine' +
          '-one/30'
        
          'a1-a2,a2-a3,a3-a4,a4-a5,a5-a6,b1-b2,b2-b3,b3-b4,b4-b5,b5-b6,c1-c' +
          '2,c2-c3,c3-c4,c4-c5,c5-c6,x-a1,x-b1,x-c1,x-a6,x-b6,x-c6')
    end
  end
end
