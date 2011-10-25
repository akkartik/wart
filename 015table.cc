COMPILE_PRIM_FUNC(table, primFunc_table, L"()",
  return mkref(newTable());
)

COMPILE_PRIM_FUNC(table_set, primFunc_table_set, L"($table $key $val)",
  Cell* table = lookup(L"$table");
  Cell* key = lookup(L"$key");
  Cell* val = lookup(L"$val");
  if (isTable(table))
    set(table, key, val);
  else
    warn << "can't set in a non-table: " << table << endl;
  return mkref(val);
)

COMPILE_PRIM_FUNC(table_get, primFunc_table_get, L"($table $key)",
  Cell* table = lookup(L"$table");
  Cell* key = lookup(L"$key");
  return mkref(get(table, key));
)
