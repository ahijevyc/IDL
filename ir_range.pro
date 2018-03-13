function IR_range, IR, typeOfIR
  case typeOfIR of
    'IR < -60C'         : IR_range = IR lt -60
    '-60C <= IR < -40C' : IR_range = -60 le IR and IR lt -40
    '-40C <= IR < -20C' : IR_range = -40 le IR and IR lt -20
    '-20C <= IR < 0C'   : IR_range = -20 le IR and IR lt 0
    'IR >= 0C'          : IR_range = IR ge 0
    'clear'             : IR_range = IR ge 0
    'mid'               : IR_range = IR ge -40 and IR lt 0
    'convection'        : IR_range = IR lt -40
    'convection-40-60C' : IR_range = IR ge -60 and IR lt -40
    'convection-60C'    : IR_range = IR lt -60
    'lt-20C'            : IR_range = IR lt -20
    'ge-20C'            : IR_range = IR ge -20
    'lt-10C'            : IR_range = IR lt -10
    'ge-10C'            : IR_range = IR ge -10
    'ge+10C'            : IR_range = IR ge +10
    'zero'              : IR_range = ~finite(IR) or finite(IR) ; used to do finite(IR), but why bother? Don't restrict if you do not care about IR.
    'all'               : IR_range = ~finite(IR) or finite(IR)
    'GFS'               : IR_range = ~finite(IR) or finite(IR)
  endcase
  return, IR_range
end

