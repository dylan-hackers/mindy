carbon-events: carbon-event-handling.lid carbon-event-handling.dylan carbon-event-handling-exports.dylan
	d2c -L../carbon carbon-event-handling.lid
	/Developer/Tools/Rez -a -o ./carbon-event-handling ../carb.r	
	/Developer/Tools/SetFile -c ???? -t APPL ./carbon-event-handling 	
clean:
	-rm -f *.o *.s *.a *.c *.mak *~ carbon-event-handling
	-rm -rf .libs
