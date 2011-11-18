COMPILE_PRIM_FUNC(table, primFunc_table, "()",
  return mkref(newTable());
)

COMPILE_PRIM_FUNC(table_set, primFunc_table_set, "($table $key $val)",
  Cell* table = lookup("$table");
  Cell* key = lookup("$key");
  Cell* val = lookup("$val");
  if (isTable(table))
    set(table, key, val);
  else
    warn << "can't set in a non-table: " << table << endl;
  return mkref(val);
)

COMPILE_PRIM_FUNC(table_get, primFunc_table_get, "($table $key)",
  Cell* table = lookup("$table");
  Cell* key = lookup("$key");
  return mkref(get(table, key));
)
