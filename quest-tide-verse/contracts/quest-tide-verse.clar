;; QuestTideVerse - Ocean-themed Metaverse Smart Contract
;; This contract manages island claims, tidal states, and tide tokens

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-claimed (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-coordinates (err u105))

;; Token configuration
(define-fungible-token tide-token)
(define-constant token-decimals u6)

;; Data Variables
(define-data-var current-tidal-phase (string-ascii 20) "high-tide")
(define-data-var last-tidal-update uint block-height)
(define-data-var tidal-cycle-blocks uint u144) ;; ~24 hours in blocks

;; Data Maps
(define-map islands
    { island-id: uint }
    {
        owner: principal,
        x-coord: int,
        y-coord: int,
        submerged: bool,
        claim-height: uint,
        resource-level: uint
    }
)

(define-map island-counter principal uint)

(define-map player-stats
    { player: principal }
    {
        islands-owned: uint,
        total-resources: uint,
        navigation-points: uint,
        last-exploration: uint
    }
)

(define-map tidal-checkpoints
    { checkpoint-id: uint }
    {
        phase: (string-ascii 20),
        block-height: uint,
        islands-affected: uint
    }
)

;; Private Functions
(define-private (is-contract-owner)
    (is-eq tx-sender contract-owner)
)

(define-private (calculate-tidal-state)
    (let
        (
            (blocks-elapsed (- block-height (var-get last-tidal-update)))
            (cycle-position (mod blocks-elapsed (var-get tidal-cycle-blocks)))
        )
        (if (< cycle-position (/ (var-get tidal-cycle-blocks) u2))
            "high-tide"
            "low-tide"
        )
    )
)

;; Read-Only Functions
(define-read-only (get-island-info (island-id uint))
    (map-get? islands { island-id: island-id })
)

(define-read-only (get-player-stats (player principal))
    (default-to
        { islands-owned: u0, total-resources: u0, navigation-points: u0, last-exploration: u0 }
        (map-get? player-stats { player: player })
    )
)

(define-read-only (get-current-tidal-phase)
    (ok (var-get current-tidal-phase))
)

(define-read-only (get-tide-token-balance (account principal))
    (ok (ft-get-balance tide-token account))
)

(define-read-only (get-token-name)
    (ok "Tide Token")
)

(define-read-only (get-token-symbol)
    (ok "TIDE")
)

(define-read-only (get-token-decimals)
    (ok token-decimals)
)

;; Public Functions

;; Initialize token supply (only owner)
(define-public (mint-tokens (amount uint) (recipient principal))
    (begin
        (asserts! (is-contract-owner) err-owner-only)
        (ft-mint? tide-token amount recipient)
    )
)

;; Claim an island
(define-public (claim-island (x-coord int) (y-coord int))
    (let
        (
            (player-current-stats (get-player-stats tx-sender))
            (new-island-id (+ (default-to u0 (map-get? island-counter tx-sender)) u1))
        )
        ;; Check if coordinates are valid (simple range check)
        (asserts! (and (>= x-coord -1000) (<= x-coord 1000)) err-invalid-coordinates)
        (asserts! (and (>= y-coord -1000) (<= y-coord 1000)) err-invalid-coordinates)
        
        ;; Create island claim
        (map-set islands
            { island-id: new-island-id }
            {
                owner: tx-sender,
                x-coord: x-coord,
                y-coord: y-coord,
                submerged: false,
                claim-height: block-height,
                resource-level: u100
            }
        )
        
        ;; Update island counter
        (map-set island-counter tx-sender new-island-id)
        
        ;; Update player stats
        (map-set player-stats
            { player: tx-sender }
            {
                islands-owned: (+ (get islands-owned player-current-stats) u1),
                total-resources: (get total-resources player-current-stats),
                navigation-points: (+ (get navigation-points player-current-stats) u10),
                last-exploration: block-height
            }
        )
        
        ;; Mint reward tokens
        (try! (ft-mint? tide-token u1000000 tx-sender))
        
        (ok new-island-id)
    )
)

;; Harvest resources from owned island
(define-public (harvest-resources (island-id uint))
    (let
        (
            (island-data (unwrap! (get-island-info island-id) err-not-found))
            (player-current-stats (get-player-stats tx-sender))
        )
        ;; Check ownership
        (asserts! (is-eq (get owner island-data) tx-sender) err-unauthorized)
        
        ;; Check if island is not submerged
        (asserts! (not (get submerged island-data)) err-not-found)
        
        ;; Calculate harvest amount based on resource level
        (let
            (
                (harvest-amount (* (get resource-level island-data) u100))
            )
            ;; Update island resources
            (map-set islands
                { island-id: island-id }
                (merge island-data { resource-level: u50 })
            )
            
            ;; Update player stats
            (map-set player-stats
                { player: tx-sender }
                (merge player-current-stats 
                    { 
                        total-resources: (+ (get total-resources player-current-stats) harvest-amount),
                        navigation-points: (+ (get navigation-points player-current-stats) u5)
                    }
                )
            )
            
            ;; Mint tide tokens as reward
            (try! (ft-mint? tide-token harvest-amount tx-sender))
            
            (ok harvest-amount)
        )
    )
)

;; Update tidal phase (can be called by anyone, simulates oracle)
(define-public (update-tidal-phase)
    (let
        (
            (new-phase (calculate-tidal-state))
            (checkpoint-id block-height)
        )
        (var-set current-tidal-phase new-phase)
        (var-set last-tidal-update block-height)
        
        ;; Record checkpoint
        (map-set tidal-checkpoints
            { checkpoint-id: checkpoint-id }
            {
                phase: new-phase,
                block-height: block-height,
                islands-affected: u0
            }
        )
        
        (ok new-phase)
    )
)

;; Toggle island submersion state based on tidal phase
(define-public (toggle-island-submersion (island-id uint))
    (let
        (
            (island-data (unwrap! (get-island-info island-id) err-not-found))
            (current-phase (var-get current-tidal-phase))
        )
        ;; Check ownership
        (asserts! (is-eq (get owner island-data) tx-sender) err-unauthorized)
        
        ;; Toggle submersion based on tidal phase
        (let
            (
                (new-submerged-state (if (is-eq current-phase "high-tide") true false))
            )
            (map-set islands
                { island-id: island-id }
                (merge island-data { submerged: new-submerged-state })
            )
            
            (ok new-submerged-state)
        )
    )
)

;; Transfer tide tokens
(define-public (transfer-tokens (amount uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-unauthorized)
        (ft-transfer? tide-token amount sender recipient)
    )
)

;; Explore and earn navigation points
(define-public (explore-ocean)
    (let
        (
            (player-current-stats (get-player-stats tx-sender))
        )
        ;; Update player stats with exploration reward
        (map-set player-stats
            { player: tx-sender }
            (merge player-current-stats 
                { 
                    navigation-points: (+ (get navigation-points player-current-stats) u20),
                    last-exploration: block-height
                }
            )
        )
        
        ;; Mint exploration reward
        (try! (ft-mint? tide-token u500000 tx-sender))
        
        (ok u20)
    )
)

;; Set tidal cycle duration (owner only)
(define-public (set-tidal-cycle (new-cycle-blocks uint))
    (begin
        (asserts! (is-contract-owner) err-owner-only)
        (var-set tidal-cycle-blocks new-cycle-blocks)
        (ok true)
    )
)