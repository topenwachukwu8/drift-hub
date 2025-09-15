;; DriftHub - Carbon Footprint Tracking Smart Contract
;; A comprehensive blockchain infrastructure for tamper-proof carbon emission monitoring

;; =============================================================================
;; ERROR CONSTANTS
;; =============================================================================

(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-SENSOR (err u101))
(define-constant ERR-INSUFFICIENT-VALIDATORS (err u102))
(define-constant ERR-SENSOR-ALREADY-REGISTERED (err u103))
(define-constant ERR-SENSOR-NOT-FOUND (err u104))
(define-constant ERR-CARBON-DNA-EXISTS (err u105))
(define-constant ERR-INVALID-EMISSION-LEVEL (err u106))
(define-constant ERR-CONSENSUS-NOT-REACHED (err u107))
(define-constant ERR-CONTRACT-PAUSED (err u108))
(define-constant ERR-INSUFFICIENT-STAKE (err u109))
(define-constant ERR-INTERVENTION-ALREADY-EXECUTED (err u110))
(define-constant ERR-INVALID-DRIFT-ZONE (err u111))
(define-constant ERR-CERTIFICATE-NOT-FOUND (err u112))
(define-constant ERR-VALIDATION-ALREADY-EXISTS (err u113))

;; =============================================================================
;; CONSTANTS
;; =============================================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-EMISSION-VARIANCE u1000)
(define-constant MIN-VALIDATORS u3)
(define-constant REPUTATION-MULTIPLIER u100)
(define-constant CARBON-DNA-FEE u1000000) ;; 1 STX
(define-constant SENSOR-REGISTRATION-FEE u500000) ;; 0.5 STX
(define-constant MAX-DRIFT-PRECISION u1000)
(define-constant MIN-CONSENSUS-THRESHOLD u51)
(define-constant MAX-CONSENSUS_THRESHOLD u100)

;; =============================================================================
;; DATA MAPS AND VARIABLES
;; =============================================================================

;; IoT sensor registry with enhanced tracking
(define-map sensor-registry 
    { sensor-id: uint }
    {
        address: principal,
        reputation-score: uint,
        total-validations: uint,
        successful-validations: uint,
        stake-amount: uint,
        is-active: bool,
        sensor-type: (string-ascii 32),
        location-hash: (buff 32),
        registration-block: uint
    }
)

;; Carbon DNA certificates with enhanced metadata
(define-map carbon-dna-certificates
    { certificate-id: (buff 32) }
    {
        emission-level: uint,
        drift-precision: uint,
        validator-count: uint,
        consensus-score: uint,
        creator: principal,
        created-at: uint,
        finalized-at: (optional uint),
        is-finalized: bool,
        metadata: (string-ascii 256),
        supply-chain-tier: uint,
        parent-certificate: (optional (buff 32))
    }
)

;; Emission validations with satellite integration
(define-map emission-validations
    { certificate-id: (buff 32), sensor-id: uint }
    {
        emission-level: uint,
        signature: (buff 65),
        satellite-data-ref: (string-ascii 64),
        validation-time: uint,
        confidence-score: uint,
        weather-adjustment: int
    }
)

;; Carbon Drift Zones for dynamic management
(define-map carbon-drift-zones
    { zone-id: uint }
    {
        min-emission-threshold: uint,
        max-emission-threshold: uint,
        penalty-rate: uint,
        reward-rate: uint,
        zone-manager: principal,
        is-active: bool,
        created-at: uint
    }
)

;; Proactive interventions with enhanced triggers
(define-map proactive-interventions
    { intervention-id: uint }
    {
        target-contract: principal,
        trigger-emission-level: uint,
        function-name: (string-ascii 64),
        is-executed: bool,
        created-by: principal,
        execution-block: (optional uint),
        intervention-type: (string-ascii 32)
    }
)

;; Supply chain relationships
(define-map supply-chain-links
    { upstream-cert: (buff 32), downstream-cert: (buff 32) }
    {
        link-strength: uint,
        carbon-transfer-ratio: uint,
        created-at: uint,
        is-active: bool
    }
)

;; Carbon offset transactions
(define-map carbon-offsets
    { offset-id: uint }
    {
        certificate-id: (buff 32),
        offset-amount: uint,
        offset-provider: principal,
        verification-status: (string-ascii 16),
        created-at: uint,
        expires-at: uint
    }
)

;; State variables
(define-data-var next-sensor-id uint u1)
(define-data-var next-intervention-id uint u1)
(define-data-var next-zone-id uint u1)
(define-data-var next-offset-id uint u1)
(define-data-var total-certificates uint u0)
(define-data-var contract-paused bool false)
(define-data-var minimum-consensus-score uint u80)
(define-data-var sensor-stake-requirement uint u10000000) ;; 10 STX
(define-data-var treasury-balance uint u0)
(define-data-var total-carbon-tracked uint u0)
(define-data-var emission-velocity-threshold uint u5000)

;; =============================================================================
;; PRIVATE FUNCTIONS
;; =============================================================================

(define-private (validate-sensor-exists (sensor-id uint))
    (is-some (map-get? sensor-registry { sensor-id: sensor-id }))
)

(define-private (is-sensor-active (sensor-id uint))
    (match (map-get? sensor-registry { sensor-id: sensor-id })
        sensor-data (get is-active sensor-data)
        false
    )
)

(define-private (update-sensor-reputation (sensor-id uint) (successful bool))
    (let ((sensor-data (unwrap! (map-get? sensor-registry { sensor-id: sensor-id }) ERR-SENSOR-NOT-FOUND)))
        (let ((new-total (+ (get total-validations sensor-data) u1))
              (new-successful (if successful 
                                (+ (get successful-validations sensor-data) u1)
                                (get successful-validations sensor-data)))
              (new-reputation (if (> new-total u0)
                               (/ (* new-successful REPUTATION-MULTIPLIER) new-total)
                               u0)))
            (map-set sensor-registry
                { sensor-id: sensor-id }
                (merge sensor-data {
                    total-validations: new-total,
                    successful-validations: new-successful,
                    reputation-score: new-reputation
                })
            )
            (ok true)
        )
    )
)

(define-private (verify-emission-drift-precision (emission-level uint) (drift-precision uint))
    (and (> emission-level u0) 
         (> drift-precision u0) 
         (<= drift-precision MAX-DRIFT-PRECISION))
)

(define-private (calculate-consensus-score (certificate-id (buff 32)))
    ;; Simplified consensus calculation based on validator agreement
    ;; In production, this would include weighted voting based on reputation
    (let ((cert-data (unwrap! (map-get? carbon-dna-certificates { certificate-id: certificate-id }) (err u0))))
        (if (>= (get validator-count cert-data) MIN-VALIDATORS)
            (ok u85) ;; Simplified: return 85% consensus
            (ok u0)
        )
    )
)

(define-private (check-drift-zone-compliance (emission-level uint) (zone-id uint))
    (match (map-get? carbon-drift-zones { zone-id: zone-id })
        zone-data 
        (and (get is-active zone-data)
             (>= emission-level (get min-emission-threshold zone-data))
             (<= emission-level (get max-emission-threshold zone-data)))
        false
    )
)

;; =============================================================================
;; ADMIN FUNCTIONS
;; =============================================================================

(define-public (pause-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (unpause-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (var-set contract-paused false)
        (ok true)
    )
)

(define-public (update-consensus-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (asserts! (and (>= new-threshold MIN-CONSENSUS-THRESHOLD) 
                      (<= new-threshold MAX-CONSENSUS_THRESHOLD)) ERR-UNAUTHORIZED)
        (var-set minimum-consensus-score new-threshold)
        (ok true)
    )
)

(define-public (update-stake-requirement (new-requirement uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (asserts! (> new-requirement u0) ERR-UNAUTHORIZED)
        (var-set sensor-stake-requirement new-requirement)
        (ok true)
    )
)

(define-public (withdraw-treasury (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (asserts! (<= amount (var-get treasury-balance)) ERR-INSUFFICIENT-STAKE)
        (try! (as-contract (stx-transfer? amount tx-sender recipient)))
        (var-set treasury-balance (- (var-get treasury-balance) amount))
        (ok true)
    )
)

;; =============================================================================
;; SENSOR MANAGEMENT
;; =============================================================================

(define-public (register-sensor 
    (stake-amount uint) 
    (sensor-type (string-ascii 32))
    (location-hash (buff 32)))
    (let ((sensor-id (var-get next-sensor-id)))
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (>= stake-amount (var-get sensor-stake-requirement)) ERR-INSUFFICIENT-STAKE)
        
        ;; Transfer stake and registration fee
        (try! (stx-transfer? (+ stake-amount SENSOR-REGISTRATION-FEE) tx-sender (as-contract tx-sender)))
        (var-set treasury-balance (+ (var-get treasury-balance) SENSOR-REGISTRATION-FEE))
        
        ;; Register sensor
        (map-set sensor-registry
            { sensor-id: sensor-id }
            {
                address: tx-sender,
                reputation-score: u100,
                total-validations: u0,
                successful-validations: u0,
                stake-amount: stake-amount,
                is-active: true,
                sensor-type: sensor-type,
                location-hash: location-hash,
                registration-block: block-height
            }
        )
        
        (var-set next-sensor-id (+ sensor-id u1))
        (ok sensor-id)
    )
)

(define-public (deactivate-sensor (sensor-id uint))
    (let ((sensor-data (unwrap! (map-get? sensor-registry { sensor-id: sensor-id }) ERR-SENSOR-NOT-FOUND)))
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                     (is-eq tx-sender (get address sensor-data))) ERR-UNAUTHORIZED)
        (map-set sensor-registry
            { sensor-id: sensor-id }
            (merge sensor-data { is-active: false })
        )
        ;; Return stake to sensor owner
        (try! (as-contract (stx-transfer? (get stake-amount sensor-data) tx-sender (get address sensor-data))))
        (ok true)
    )
)

(define-public (slash-sensor (sensor-id uint) (slash-amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (let ((sensor-data (unwrap! (map-get? sensor-registry { sensor-id: sensor-id }) ERR-SENSOR-NOT-FOUND)))
            (asserts! (<= slash-amount (get stake-amount sensor-data)) ERR-INSUFFICIENT-STAKE)
            (map-set sensor-registry
                { sensor-id: sensor-id }
                (merge sensor-data { 
                    stake-amount: (- (get stake-amount sensor-data) slash-amount),
                    is-active: (> (- (get stake-amount sensor-data) slash-amount) (var-get sensor-stake-requirement))
                })
            )
            (var-set treasury-balance (+ (var-get treasury-balance) slash-amount))
            (ok true)
        )
    )
)

;; =============================================================================
;; CARBON DNA CERTIFICATES
;; =============================================================================

(define-public (create-carbon-dna-certificate 
    (certificate-id (buff 32))
    (emission-level uint)
    (drift-precision uint)
    (metadata (string-ascii 256))
    (supply-chain-tier uint)
    (parent-certificate (optional (buff 32))))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (is-none (map-get? carbon-dna-certificates { certificate-id: certificate-id })) 
                  ERR-CARBON-DNA-EXISTS)
        (asserts! (verify-emission-drift-precision emission-level drift-precision) ERR-INVALID-EMISSION-LEVEL)
        
        ;; Verify parent certificate exists if specified
        (match parent-certificate
            parent-id (asserts! (is-some (map-get? carbon-dna-certificates { certificate-id: parent-id })) 
                               ERR-CERTIFICATE-NOT-FOUND)
            true
        )
        
        ;; Charge fee
        (try! (stx-transfer? CARBON-DNA-FEE tx-sender (as-contract tx-sender)))
        (var-set treasury-balance (+ (var-get treasury-balance) CARBON-DNA-FEE))
        
        ;; Create certificate
        (map-set carbon-dna-certificates
            { certificate-id: certificate-id }
            {
                emission-level: emission-level,
                drift-precision: drift-precision,
                validator-count: u0,
                consensus-score: u0,
                creator: tx-sender,
                created-at: block-height,
                finalized-at: none,
                is-finalized: false,
                metadata: metadata,
                supply-chain-tier: supply-chain-tier,
                parent-certificate: parent-certificate
            }
        )
        
        (var-set total-certificates (+ (var-get total-certificates) u1))
        (var-set total-carbon-tracked (+ (var-get total-carbon-tracked) emission-level))
        (ok true)
    )
)

(define-public (submit-emission-validation
    (certificate-id (buff 32))
    (sensor-id uint)
    (emission-level uint)
    (signature (buff 65))
    (satellite-data-ref (string-ascii 64))
    (confidence-score uint)
    (weather-adjustment int))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (validate-sensor-exists sensor-id) ERR-SENSOR-NOT-FOUND)
        (asserts! (is-sensor-active sensor-id) ERR-SENSOR-NOT-FOUND)
        (asserts! (is-some (map-get? carbon-dna-certificates { certificate-id: certificate-id })) 
                  ERR-CERTIFICATE-NOT-FOUND)
        (asserts! (<= confidence-score u100) ERR-INVALID-EMISSION-LEVEL)
        
        ;; Ensure validation doesn't already exist
        (asserts! (is-none (map-get? emission-validations { certificate-id: certificate-id, sensor-id: sensor-id }))
                  ERR-VALIDATION-ALREADY-EXISTS)
        
        ;; Store validation
        (map-set emission-validations
            { certificate-id: certificate-id, sensor-id: sensor-id }
            {
                emission-level: emission-level,
                signature: signature,
                satellite-data-ref: satellite-data-ref,
                validation-time: block-height,
                confidence-score: confidence-score,
                weather-adjustment: weather-adjustment
            }
        )
        
        ;; Update sensor reputation
        (try! (update-sensor-reputation sensor-id true))
        
        ;; Update certificate validator count
        (let ((cert-data (unwrap! (map-get? carbon-dna-certificates { certificate-id: certificate-id }) ERR-CERTIFICATE-NOT-FOUND)))
            (map-set carbon-dna-certificates
                { certificate-id: certificate-id }
                (merge cert-data { 
                    validator-count: (+ (get validator-count cert-data) u1) 
                })
            )
        )
        
        (ok true)
    )
)

(define-public (finalize-certificate (certificate-id (buff 32)))
    (let ((cert-data (unwrap! (map-get? carbon-dna-certificates { certificate-id: certificate-id }) ERR-CERTIFICATE-NOT-FOUND)))
        (asserts! (not (get is-finalized cert-data)) ERR-CARBON-DNA-EXISTS)
        (asserts! (>= (get validator-count cert-data) MIN-VALIDATORS) ERR-INSUFFICIENT-VALIDATORS)
        
        (let ((consensus-result (unwrap! (calculate-consensus-score certificate-id) ERR-CONSENSUS-NOT-REACHED)))
            (asserts! (>= consensus-result (var-get minimum-consensus-score)) ERR-CONSENSUS-NOT-REACHED)
            (map-set carbon-dna-certificates
                { certificate-id: certificate-id }
                (merge cert-data { 
                    consensus-score: consensus-result,
                    is-finalized: true,
                    finalized-at: (some block-height)
                })
            )
            (ok true)
        )
    )
)

;; =============================================================================
;; CARBON DRIFT ZONES
;; =============================================================================

(define-public (create-drift-zone
    (min-threshold uint)
    (max-threshold uint)
    (penalty-rate uint)
    (reward-rate uint))
    (let ((zone-id (var-get next-zone-id)))
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (< min-threshold max-threshold) ERR-INVALID-DRIFT-ZONE)
        (asserts! (and (<= penalty-rate u100) (<= reward-rate u100)) ERR-INVALID-DRIFT-ZONE)
        
        (map-set carbon-drift-zones
            { zone-id: zone-id }
            {
                min-emission-threshold: min-threshold,
                max-emission-threshold: max-threshold,
                penalty-rate: penalty-rate,
                reward-rate: reward-rate,
                zone-manager: tx-sender,
                is-active: true,
                created-at: block-height
            }
        )
        
        (var-set next-zone-id (+ zone-id u1))
        (ok zone-id)
    )
)

;; =============================================================================
;; SUPPLY CHAIN MANAGEMENT
;; =============================================================================

(define-public (link-supply-chain
    (upstream-cert (buff 32))
    (downstream-cert (buff 32))
    (link-strength uint)
    (carbon-transfer-ratio uint))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (is-some (map-get? carbon-dna-certificates { certificate-id: upstream-cert }))
                  ERR-CERTIFICATE-NOT-FOUND)
        (asserts! (is-some (map-get? carbon-dna-certificates { certificate-id: downstream-cert }))
                  ERR-CERTIFICATE-NOT-FOUND)
        (asserts! (and (<= link-strength u100) (<= carbon-transfer-ratio u100))
                  ERR-INVALID-EMISSION-LEVEL)
        
        (map-set supply-chain-links
            { upstream-cert: upstream-cert, downstream-cert: downstream-cert }
            {
                link-strength: link-strength,
                carbon-transfer-ratio: carbon-transfer-ratio,
                created-at: block-height,
                is-active: true
            }
        )
        (ok true)
    )
)

;; =============================================================================
;; PROACTIVE INTERVENTIONS
;; =============================================================================

(define-public (register-proactive-intervention
    (target-contract principal)
    (trigger-emission-level uint)
    (function-name (string-ascii 64))
    (intervention-type (string-ascii 32)))
    (let ((intervention-id (var-get next-intervention-id)))
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (> trigger-emission-level u0) ERR-INVALID-EMISSION-LEVEL)
        
        (map-set proactive-interventions
            { intervention-id: intervention-id }
            {
                target-contract: target-contract,
                trigger-emission-level: trigger-emission-level,
                function-name: function-name,
                is-executed: false,
                created-by: tx-sender,
                execution-block: none,
                intervention-type: intervention-type
            }
        )
        
        (var-set next-intervention-id (+ intervention-id u1))
        (ok intervention-id)
    )
)

(define-public (execute-proactive-intervention (intervention-id uint) (current-emission-level uint))
    (let ((intervention-data (unwrap! (map-get? proactive-interventions { intervention-id: intervention-id }) ERR-UNAUTHORIZED)))
        (asserts! (not (get is-executed intervention-data)) ERR-INTERVENTION-ALREADY-EXECUTED)
        (asserts! (>= current-emission-level (get trigger-emission-level intervention-data)) ERR-UNAUTHORIZED)
        
        (map-set proactive-interventions
            { intervention-id: intervention-id }
            (merge intervention-data { 
                is-executed: true,
                execution-block: (some block-height)
            })
        )
        (ok true)
    )
)

;; =============================================================================
;; CARBON OFFSET MANAGEMENT
;; =============================================================================

(define-public (create-carbon-offset
    (certificate-id (buff 32))
    (offset-amount uint)
    (expires-at uint))
    (let ((offset-id (var-get next-offset-id)))
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (asserts! (is-some (map-get? carbon-dna-certificates { certificate-id: certificate-id }))
                  ERR-CERTIFICATE-NOT-FOUND)
        (asserts! (> expires-at block-height) ERR-UNAUTHORIZED)
        
        (map-set carbon-offsets
            { offset-id: offset-id }
            {
                certificate-id: certificate-id,
                offset-amount: offset-amount,
                offset-provider: tx-sender,
                verification-status: "pending",
                created-at: block-height,
                expires-at: expires-at
            }
        )
        
        (var-set next-offset-id (+ offset-id u1))
        (ok offset-id)
    )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

(define-read-only (get-sensor-info (sensor-id uint))
    (map-get? sensor-registry { sensor-id: sensor-id })
)

(define-read-only (get-certificate-info (certificate-id (buff 32)))
    (map-get? carbon-dna-certificates { certificate-id: certificate-id })
)

(define-read-only (get-validation-info (certificate-id (buff 32)) (sensor-id uint))
    (map-get? emission-validations { certificate-id: certificate-id, sensor-id: sensor-id })
)

(define-read-only (get-intervention-info (intervention-id uint))
    (map-get? proactive-interventions { intervention-id: intervention-id })
)

(define-read-only (get-drift-zone-info (zone-id uint))
    (map-get? carbon-drift-zones { zone-id: zone-id })
)

(define-read-only (get-supply-chain-link (upstream-cert (buff 32)) (downstream-cert (buff 32)))
    (map-get? supply-chain-links { upstream-cert: upstream-cert, downstream-cert: downstream-cert })
)

(define-read-only (get-carbon-offset-info (offset-id uint))
    (map-get? carbon-offsets { offset-id: offset-id })
)

(define-read-only (get-contract-stats)
    {
        total-certificates: (var-get total-certificates),
        total-sensors: (- (var-get next-sensor-id) u1),
        total-interventions: (- (var-get next-intervention-id) u1),
        total-drift-zones: (- (var-get next-zone-id) u1),
        treasury-balance: (var-get treasury-balance),
        total-carbon-tracked: (var-get total-carbon-tracked),
        is-paused: (var-get contract-paused),
        minimum-consensus-score: (var-get minimum-consensus-score),
        sensor-stake-requirement: (var-get sensor-stake-requirement)
    }
)

(define-read-only (check-emission-velocity (certificate-id (buff 32)) (projected-emission uint))
    (match (map-get? carbon-dna-certificates { certificate-id: certificate-id })
        cert-data 
        (let ((current-emission (get emission-level cert-data))
              (velocity (if (> projected-emission current-emission)
                          (- projected-emission current-emission)
                          (- current-emission projected-emission))))
            {
                velocity: velocity,
                exceeds-threshold: (> velocity (var-get emission-velocity-threshold)),
                recommendation: (if (> velocity (var-get emission-velocity-threshold))
                                  "trigger-intervention"
                                  "continue-monitoring")
            }
        )
        {
            velocity: u0,
            exceeds-threshold: false,
            recommendation: "certificate-not-found"
        }
    )
)

(define-read-only (get-certificate-carbon-dna (certificate-id (buff 32)))
    (match (map-get? carbon-dna-certificates { certificate-id: certificate-id })
        cert-data
        {
            certificate-id: certificate-id,
            emission-level: (get emission-level cert-data),
            drift-precision: (get drift-precision cert-data),
            consensus-score: (get consensus-score cert-data),
            is-finalized: (get is-finalized cert-data),
            supply-chain-tier: (get supply-chain-tier cert-data),
            carbon-dna-hash: certificate-id
        }
        {
            certificate-id: certificate-id,
            emission-level: u0,
            drift-precision: u0,
            consensus-score: u0,
            is-finalized: false,
            supply-chain-tier: u0,
            carbon-dna-hash: 0x00
        }
    )
)