COMPILE_FN(table, compiledFn_table, "()",
  return mkref(newTable());
)

COMPILE_FN(table_set, compiledFn_table_set, "($table $key $val)",
  Cell* table = lookup("$table");
  Cell* key = lookup("$key");
  Cell* val = lookup("$val");
  if (isTable(table))
    set(table, key, val);
  else
    RAISE << "can't set in a non-table: " << table << endl;
  return mkref(val);
)

COMPILE_FN(table_get, compiledFn_table_get, "($table $key)",
  Cell* table = lookup("$table");
  Cell* key = lookup("$key");
  return mkref(get(table, key));
)

COMPILE_FN(table_to_list, compiledFn_table_to_list, "($table)",
  CellMap table = toTable(lookup("$table"))->table;
  Cell* result = newCell();
  Cell* curr = result;
  for (CellMap::iterator p = table.begin(); p != table.end(); ++p) {
    if (!p->second) continue;
    addCons(curr, newCons((Cell*)p->first, newCons((Cell*)p->second, nil)));
    curr=cdr(curr);
  }
  return dropPtr(result);
)
