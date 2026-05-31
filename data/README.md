# Data folder

- `raw/`: immutable source files downloaded from official providers (e.g., BCB SGS API exports).
- `processed/`: cleaned and transformed analysis-ready files.

Keep raw data unchanged after ingestion; transformations should be reproducible via code.

# Data codebook

This project uses monthly credit statistics from the Banco Central do Brasil SGS system.

## SGS codebook

### Credit outstanding

| Code | Description |
|---:|---|
| 20539 | Credit outstanding - total |
| 20540 | Credit outstanding - non-financial corporations - total |
| 20541 | Credit outstanding - households - total |
| 20542 | Nonearmarked credit outstanding - total |
| 20543 | Nonearmarked credit outstanding - nonfinancial corporations - total |
| 20570 | Nonearmarked credit outstanding - households - total |
| 20593 | Earmarked credit outstanding - total |
| 20594 | Earmarked credit outstanding - nonfinancial corporations - total |
| 20606 | Earmarked credit outstanding - households - total |

### New-operation interest rates

| Code | Description |
|---:|---|
| 25433 | Month average interest rate - new operations - total |
| 27638 | Month average interest rate - new non-revolving operations - total |
| 25434 | Month average interest rate - new operations - non-financial corporations - total |
| 27639 | Month average interest rate - new non-revolving operations - non-financial corporations - total |
| 25435 | Month average interest rate - new operations - individual persons - total |
| 27640 | Month average interest rate - new non-revolving operations - individual persons - total |
| 25436 | Month average interest rate - nonearmarked new operations - total |
| 25437 | Month average interest rate - nonearmarked new operations - non-financial corporations - total |
| 25462 | Month average interest rate - nonearmarked new operations - households - total |
| 25481 | Month average interest rate - earmarked new operations - total |
| 25482 | Month average interest rate - earmarked new operations - non-financial corporations - total |
| 25493 | Month average interest rate - earmarked new operations - households - total |

### Average cost of outstanding loans — ICC

| Code | Description |
|---:|---|
| 25351 | Average cost of outstanding loans, ICC - total |
| 25352 | Average cost of outstanding loans, ICC - nonfinancial corporations - total |
| 25353 | Average cost of outstanding loans, ICC - individuals - total |

### Monetary policy

| Code | Description |
|---:|---|
| 432 | Selic target |
| 422 | Central Bank base rate (TBC) in annual terms |
| 423 | Central Bank assistance rate (TBAN) in annual terms |
| 13521 | Inflation target |

### Inflation

| Code | Description |
|---:|---|
| 433 | IPCA |
| 13522 | IPCA - in 12 months |

### Activity

| Code | Description |
|---:|---|
| 21859 | Industrial Output - General (2022 = 100) |
| 4380 | GDP monthly - current prices |

### Exchange rate

| Code | Description |
|---:|---|
| 10813 | Commercial exchange rate (buy), c.m.u./US$ |