{$DEFINE E_NOT_IMPLEMENTED := RuntimeError.Create(eNotImplemented)}
{$DEFINE E_NOT_COMPATIBLE1 := RuntimeError.CreateFmt(eNotCompatible1, [self.ToString, other.ToString])}