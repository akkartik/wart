//// compiled primitives for tables

COMPILE_FN(table, compiledfn_table, "()",
  return mkref(new_table());
)

COMPILE_FN(table_set, compiledfn_table_set, "($table $key $val)",
  cell* table = lookup("$table");
  cell* key = lookup("$key");
  cell* val = lookup("$val");
  if (is_table(table))
    put(table, key, val);
  else
    RAISE << "can't set in a non-table: " << table << '\n';
  return mkref(val);
)

COMPILE_FN(table_get, compiledfn_table_get, "($table $key)",
  cell* table = lookup("$table");
  cell* key = lookup("$key");
  return mkref(get(table, key));
)

COMPILE_FN(table_to_list, compiledfn_table_to_list, "($table)",
  cell_map table = to_table(lookup("$table"))->value;
  cell* p_result = new_cell();  cell* curr = p_result;
  for (cell_map::iterator p = table.begin(); p != table.end(); ++p) {
    if (!p->second) continue;
    add_cons(curr, new_cons(p->first, new_cons(p->second)));
    curr=cdr(curr);
  }
  return drop_ptr(p_result);
)

COMPILE_FN(table_length, compiledfn_table_length, "($table)",
  cell_map table = to_table(lookup("$table"))->value;
  return mkref(new_num((long)table.size()));
)
