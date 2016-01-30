FOR EACH billChqAssoc.
    DELETE billChqAssoc.
END.
/* FOR EACH bills.   */
/*     DELETE bills. */
/* END.              */
FOR EACH BSSave.
    DELETE BSSave.
END.
FOR EACH cheques.
    DELETE cheques.
END.
FOR EACH lorryStock.
    DELETE lorryStock.
END.
FOR EACH Payments.
    DELETE Payments.
END.
FOR EACH recipts.
    DELETE recipts.
END.

FOR EACH itms.
	IF itmID = 21 or itmID = 171 or itmID = 23 or itmID = 24 or itmID = 142 THEN
	DO:
		itms.noOfCases = 10.
		itms.stockC    = 10.
        itms.noOfUnits = 0.
		itms.stockP    = 0.
	END.
	ELSE
	DO:
		itms.noOfCases = 0.
		itms.noOfUnits = 0.
		itms.stockP    = 0.
		itms.stockC    = 0.
	END.
END.

/* FOR EACH area.   */
/*     DELETE area. */
/* END.             */
/* FOR EACH customer.   */
/*     DELETE customer. */
/* END.                 */
/* FOR EACH CusCat.     */
/*     DELETE CusCat.   */
/* END.                 */
/* FOR EACH itemCat.   */
/*     DELETE itemCat. */
/* END.                */
/* FOR EACH itms.   */
/*     DELETE itms. */
/* END.             */
FOR EACH Payments.
    DELETE Payments.
END.
/* FOR EACH userCat.   */
/*     DELETE userCat. */
/* END.                */
/* FOR EACH vehical.   */
/*     DELETE vehical. */
/* END.                */
