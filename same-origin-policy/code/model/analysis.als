/**
  *  analysis.als
  *    End-to-end analysis of security properties
  */
module analysis
open flow

// General security properties
sig TrustedModule, MaliciousModule in DataflowModule {}
sig CriticalData, MaliciousData in Data {}

// No malicious module should be able to access private data
assert Confidentiality {
  no m: DataflowModule - TrustedModule, t: Time |
    some CriticalData & m.accesses.t 
}

// No malicious data should ever flow into a trusted module
assert Integrity {
  no u: TrustedModule, t: Time | 
    some MaliciousData & u.accesses.t
}

fact  {
  -- no malicious module begins with critical data
  no m: MaliciousModule | some CriticalData & m.accesses.init
  -- no trusted module begins with malicious data
  no m: TrustedModule | some MaliciousData & m.accesses.init
  -- no module is both trusted and malicious
  no TrustedModule & MaliciousModule
  -- every module is either trusted or malicious
  DataflowModule = TrustedModule + MaliciousModule
  -- no data is both critical and malicious
  no CriticalData & MaliciousData
}

check Confidentiality for 3
check Integrity for 3
