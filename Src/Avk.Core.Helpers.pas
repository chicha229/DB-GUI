unit Avk.Core.Helpers;

interface

uses
  System.Generics.Collections, System.Classes;

type
  TArrayHelper = class helper for TArray
  public
    class procedure Add<T>(A: System.TArray<T>; V: T);
  end;

implementation

{ TArrayHelper<T> }

class procedure TArrayHelper.Add<T>(A: System.TArray<T>; V: T);
begin
  SetLength(A, Length(A) + 1);
  A[Length(A) - 1] := V;
end;

end.
