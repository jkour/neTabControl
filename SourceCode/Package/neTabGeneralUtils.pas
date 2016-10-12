//***************************************************************
// This source is written by John Kouraklis.
// © 2016, John Kouraklis
// Email : j_kour@hotmail.com
//
// The MIT License (MIT)
//
// Copyright (c) 2016 John Kouraklis
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
// KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
// WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// Unit Name: neTabGeneralUtils
//
//
//
//***************************************************************

unit neTabGeneralUtils;

interface

uses
  System.Classes, FMX.Graphics, FMX.Types, FMX.Forms;

//////////////////////////////////////////////////
/// For version info, please see the .inc file ///
//////////////////////////////////////////////////

{$I neTabControlVersionInfo.inc}

procedure LoadImageFromResources(const resName: string; var image: TBitmap);

function FindParentForm(const checkComponent: TFmxObject):TForm;

implementation

uses
  System.Types;

procedure LoadImageFromResources(const resName: string; var image: TBitmap);
var
  InStream: TResourceStream;
begin
  InStream := TResourceStream.Create(HInstance, resName, RT_RCDATA);
  try
    Image.LoadFromStream(InStream);
  finally
    InStream.Free;
  end;
end;

function FindParentForm(const checkComponent: TFmxObject):TForm;
begin
  if not Assigned(checkComponent.Parent) then
    result:=checkComponent as TForm
  else
    if checkComponent.Parent Is TForm then
      result:=checkComponent.Parent as TForm
    else
      result:=FindParentForm(checkComponent.Parent.Parent);
end;


end.
