 ;; Commodity Warehouse Token Contract
;; Tokenizing physical commodities with warehouse receipts for liquid markets

;; Define the commodity warehouse token
(define-fungible-token commodity-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-commodity-not-found (err u104))
(define-constant err-invalid-commodity-type (err u105))

;; Token metadata
(define-data-var token-name (string-ascii 32) "Commodity Warehouse Token")
(define-data-var token-symbol (string-ascii 10) "CWT")
(define-data-var token-decimals uint u6)

;; Commodity types
(define-constant COMMODITY-GOLD u1)
(define-constant COMMODITY-OIL u2)
(define-constant COMMODITY-GRAIN u3)

;; Warehouse receipt structure
(define-map warehouse-receipts
  uint ;; receipt-id
  {
    commodity-type: uint,
    quantity: uint,
    warehouse-location: (string-ascii 50),
    deposit-date: uint,
    owner: principal,
    is-active: bool
  })

;; Commodity balances by type
(define-map commodity-balances
  {owner: principal, commodity-type: uint}
  uint)

;; Receipt counter
(define-data-var receipt-counter uint u0)

;; Total supply by commodity type
(define-map total-supply-by-commodity uint uint)

;; Function 1: Deposit Physical Commodity and Issue Warehouse Receipt
(define-public (deposit-commodity 
  (commodity-type uint) 
  (quantity uint) 
  (warehouse-location (string-ascii 50)))
  (let 
    (
      (receipt-id (+ (var-get receipt-counter) u1))
      (current-balance (default-to u0 (map-get? commodity-balances {owner: tx-sender, commodity-type: commodity-type})))
      (current-total (default-to u0 (map-get? total-supply-by-commodity commodity-type)))
    )
    ;; Validate inputs
    (asserts! (> quantity u0) err-invalid-amount)
    (asserts! (or (is-eq commodity-type COMMODITY-GOLD) 
                  (is-eq commodity-type COMMODITY-OIL) 
                  (is-eq commodity-type COMMODITY-GRAIN)) err-invalid-commodity-type)
    
    ;; Create warehouse receipt
    (map-set warehouse-receipts receipt-id
      {
        commodity-type: commodity-type,
        quantity: quantity,
        warehouse-location: warehouse-location,
        deposit-date: stacks-block-height,
        owner: tx-sender,
        is-active: true
      })
    
    ;; Update balances
    (map-set commodity-balances 
      {owner: tx-sender, commodity-type: commodity-type}
      (+ current-balance quantity))
    
    ;; Update total supply for commodity type
    (map-set total-supply-by-commodity commodity-type (+ current-total quantity))
    
    ;; Mint equivalent tokens
    (try! (ft-mint? commodity-token quantity tx-sender))
    
    ;; Increment receipt counter
    (var-set receipt-counter receipt-id)
    
    ;; Print event for off-chain tracking
    (print {
      action: "commodity-deposited",
      receipt-id: receipt-id,
      commodity-type: commodity-type,
      quantity: quantity,
      owner: tx-sender,
      warehouse-location: warehouse-location
    })
    
    (ok receipt-id)))

;; Function 2: Withdraw Physical Commodity Using Warehouse Receipt
(define-public (withdraw-commodity (receipt-id uint))
  (let 
    (
      (receipt-data (unwrap! (map-get? warehouse-receipts receipt-id) err-commodity-not-found))
      (commodity-type (get commodity-type receipt-data))
      (quantity (get quantity receipt-data))
      (receipt-owner (get owner receipt-data))
      (is-active (get is-active receipt-data))
      (current-balance (default-to u0 (map-get? commodity-balances {owner: tx-sender, commodity-type: commodity-type})))
      (current-total (default-to u0 (map-get? total-supply-by-commodity commodity-type)))
    )
    
    ;; Validate withdrawal
    (asserts! is-active err-commodity-not-found)
    (asserts! (is-eq tx-sender receipt-owner) err-not-authorized)
    (asserts! (>= current-balance quantity) err-insufficient-balance)
    (asserts! (>= (ft-get-balance commodity-token tx-sender) quantity) err-insufficient-balance)
    
    ;; Deactivate warehouse receipt
    (map-set warehouse-receipts receipt-id
      (merge receipt-data {is-active: false}))
    
    ;; Update balances
    (map-set commodity-balances 
      {owner: tx-sender, commodity-type: commodity-type}
      (- current-balance quantity))
    
    ;; Update total supply for commodity type
    (map-set total-supply-by-commodity commodity-type (- current-total quantity))
    
    ;; Burn equivalent tokens
    (try! (ft-burn? commodity-token quantity tx-sender))
    
    ;; Print event for off-chain tracking
    (print {
      action: "commodity-withdrawn",
      receipt-id: receipt-id,
      commodity-type: commodity-type,
      quantity: quantity,
      owner: tx-sender
    })
    
    (ok true)))

;; Read-only functions for querying

;; Get warehouse receipt details
(define-read-only (get-warehouse-receipt (receipt-id uint))
  (ok (map-get? warehouse-receipts receipt-id)))

;; Get commodity balance for user by type
(define-read-only (get-commodity-balance (owner principal) (commodity-type uint))
  (ok (default-to u0 (map-get? commodity-balances {owner: owner, commodity-type: commodity-type}))))

;; Get total supply by commodity type
(define-read-only (get-total-supply-by-commodity (commodity-type uint))
  (ok (default-to u0 (map-get? total-supply-by-commodity commodity-type))))

;; Get token balance
(define-read-only (get-token-balance (account principal))
  (ok (ft-get-balance commodity-token account)))

;; Get current receipt counter
(define-read-only (get-current-receipt-id)
  (ok (var-get receipt-counter)))