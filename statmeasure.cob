*> Kabir Kalia, 1170812, March 27th, 2024, CIS 3190.

IDENTIFICATION DIVISION.
PROGRAM-ID. statmeasure.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO DYNAMIC user-input-file ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS fs-status.  *> Handling file status to check for errors during file operations.

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 INPUT-RECORD.
    02 NUMERIC-DATA PIC S9(6)V9(2).  *> Defines the structure to read numeric data with two decimal places.
    02 FILLER PIC X(72).  *> Filler to accommodate the rest of the line.

WORKING-STORAGE SECTION.
01 user-input-file PIC X(50).  *> Variable to store the user-specified file name.
01 fs-status PIC XX.  *> Variable to capture the file status after operations.
01 total-numbers PIC S9(4) VALUE ZERO.  *> Counter for the total number of records processed.
01 aggregate PIC 9(14)V9(2) VALUE ZERO.  *> Sum of all the numbers processed.
01 sum-of-reciprocals PIC 9(14)V9(8) VALUE 0.  *> Sum of the reciprocals for HM calculation.
01 total-squares PIC 9(14)V9(2) VALUE 0.  *> Sum of the squares of all numbers for RMS and variance.
01 log-total PIC S9(14)V9(8) VALUE 0.  *> Sum of the logs of all numbers for GM calculation.
01 variance PIC 9(14)V9(8) VALUE 0.  *> Variance of the numbers for standard deviation calculation.
01 geometric-mean PIC S9(6)V9(2) VALUE ZERO.  *> Geometric mean result.
01 harmonic-mean PIC S9(6)V9(2) VALUE ZERO.  *> Harmonic mean result.
01 rms PIC S9(6)V9(2) VALUE ZERO.  *> Root mean square result.
01 mean PIC S9(6)V9(2) VALUE ZERO.  *> Mean result.
01 std-deviation PIC S9(6)V9(2) VALUE ZERO.  *> Standard deviation result.
01 formatted-sum PIC ZZZZ9.99.  *> For displaying sum without leading zeros.
01 formatted-mean PIC ZZZZ9.99.  *> For displaying mean without leading zeros.
01 formatted-std-dev PIC ZZZZ9.99.  *> For displaying standard deviation without leading zeros.
01 formatted-geo-mean PIC ZZZZ9.99.  *> For displaying geometric mean without leading zeros.
01 formatted-har-mean PIC ZZZZ9.99.  *> For displaying harmonic mean without leading zeros.
01 formatted-rms PIC ZZZZ9.99.  *> For displaying RMS without leading zeros.
01 eof-indicator PIC 9 VALUE 0.
    88 eof VALUE 1.  *> Flag to indicate end-of-file reached.

PROCEDURE DIVISION.
000-INITIALIZE.
    DISPLAY "Enter the name of the input file: ".
    ACCEPT user-input-file.
    OPEN INPUT INPUT-FILE.

    EVALUATE fs-status
        WHEN '00'
            CONTINUE
        WHEN OTHER
            DISPLAY "Error opening file: ", user-input-file, " - Status: ", fs-status
            CLOSE INPUT-FILE
            STOP RUN
    END-EVALUATE.  *> Check file status and report error if any.

    PERFORM UNTIL eof
        READ INPUT-FILE INTO INPUT-RECORD
            AT END
                SET eof TO TRUE
            NOT AT END
                ADD 1 TO total-numbers
                COMPUTE aggregate = aggregate + NUMERIC-DATA
                COMPUTE sum-of-reciprocals = sum-of-reciprocals + (1 / NUMERIC-DATA)
                COMPUTE total-squares = total-squares + (NUMERIC-DATA ** 2)
                COMPUTE log-total = log-total + FUNCTION LOG(NUMERIC-DATA)
        END-READ
    END-PERFORM.  *> Main loop to read and process each record.

    COMPUTE mean = aggregate / total-numbers.
    COMPUTE variance = (total-squares - (aggregate ** 2 / total-numbers)) / total-numbers.
    COMPUTE std-deviation = FUNCTION SQRT(variance).
    COMPUTE geometric-mean = FUNCTION EXP(log-total / total-numbers).
    COMPUTE harmonic-mean = total-numbers / sum-of-reciprocals.
    COMPUTE rms = FUNCTION SQRT(total-squares / total-numbers).

    MOVE aggregate TO formatted-sum.
    MOVE mean TO formatted-mean.
    MOVE std-deviation TO formatted-std-dev.
    MOVE geometric-mean TO formatted-geo-mean.
    MOVE harmonic-mean TO formatted-har-mean.
    MOVE rms TO formatted-rms.

    DISPLAY "SUM = ", formatted-sum.
    DISPLAY "MEAN = ", formatted-mean.
    DISPLAY "STANDARD DEV = ", formatted-std-dev.
    DISPLAY "Geometric mean = ", formatted-geo-mean.
    DISPLAY "Harmonic mean = ", formatted-har-mean.
    DISPLAY "RMS = ", formatted-rms.  *> Display the calculated statistics.

    CLOSE INPUT-FILE.
    STOP RUN.
