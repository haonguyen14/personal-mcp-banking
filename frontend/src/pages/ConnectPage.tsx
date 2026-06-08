import { useEffect, useState } from 'react'
import { usePlaidLink } from 'react-plaid-link'
import { Separator } from 'radix-ui'
import type { BankingEngine } from '../engine'

type Status = 'checking' | 'disconnected' | 'connecting' | 'connected' | 'error'

const MOCK_LINK_TOKEN = 'mock-link-token'

function getOAuthStateId(): string | null {
  return new URLSearchParams(window.location.search).get('oauth_state_id')
}

interface Props {
  engine: BankingEngine
}

export default function ConnectPage({ engine }: Props) {
  const [status, setStatus] = useState<Status>('checking')
  const [linkToken, setLinkToken] = useState<string | null>(null)
  const [errorMsg, setErrorMsg] = useState<string | null>(null)

  const isOAuthReturn = getOAuthStateId() !== null

  useEffect(() => {
    engine.checkConnection()
      .then(({ connected }) => setStatus(connected ? 'connected' : 'disconnected'))
      .catch(() => setStatus('disconnected'))
  }, [engine])

  // On OAuth return, fetch a fresh link token to re-initialize Plaid Link
  useEffect(() => {
    if (!isOAuthReturn) return
    setStatus('connecting')
    engine.getLinkToken()
      .then(setLinkToken)
      .catch(() => {
        setErrorMsg('Failed to resume connection. Please try again.')
        setStatus('error')
      })
  }, [isOAuthReturn, engine])

  const { open, ready } = usePlaidLink({
    token: linkToken,
    receivedRedirectUri: isOAuthReturn ? window.location.href : undefined,
    onSuccess: async (publicToken) => {
      try {
        await engine.exchangePublicToken(publicToken)
        // Clear oauth params from URL without reloading
        window.history.replaceState({}, '', window.location.pathname)
        setStatus('connected')
      } catch {
        setErrorMsg('Failed to exchange token. Please try again.')
        setStatus('error')
      }
    },
    onExit: () => {
      setStatus('disconnected')
      setLinkToken(null)
    },
  })

  useEffect(() => {
    if (linkToken && linkToken !== MOCK_LINK_TOKEN && ready) {
      open()
    }
  }, [linkToken, ready, open])

  const handleConnect = async () => {
    setStatus('connecting')
    setErrorMsg(null)
    try {
      const token = await engine.getLinkToken()
      if (token === MOCK_LINK_TOKEN) {
        await engine.exchangePublicToken('mock-public-token')
        setStatus('connected')
      } else {
        setLinkToken(token)
      }
    } catch {
      setErrorMsg('Failed to start connection. Please try again.')
      setStatus('error')
    }
  }

  return (
    <div className="min-h-screen bg-gray-50 flex items-center justify-center p-4">
      <div className="bg-white rounded-2xl shadow-sm border border-gray-200 w-full max-w-md p-8">

        <div className="mb-6">
          <h1 className="text-xl font-semibold text-gray-900">Bank Connection</h1>
          <p className="text-sm text-gray-500 mt-1">
            Connect your bank account to enable spending analysis via Claude.
          </p>
        </div>

        <Separator.Root className="h-px bg-gray-100 mb-6" />

        <div className="flex items-center gap-3 mb-6">
          <StatusDot status={status} />
          <StatusLabel status={status} isOAuthReturn={isOAuthReturn} />
        </div>

        {status === 'connected' ? (
          <div className="rounded-lg bg-green-50 border border-green-200 p-4 text-sm text-green-800">
            Your bank account is connected. Claude can now access your transactions.
          </div>
        ) : (
          <>
            <button
              onClick={handleConnect}
              disabled={status === 'checking' || status === 'connecting' || isOAuthReturn}
              className="w-full bg-gray-900 text-white rounded-lg py-2.5 text-sm font-medium
                         hover:bg-gray-700 disabled:opacity-40 disabled:cursor-not-allowed
                         transition-colors duration-150"
            >
              {status === 'connecting' ? 'Connecting…' : 'Connect Bank Account'}
            </button>

            {errorMsg && (
              <p className="mt-3 text-sm text-red-600">{errorMsg}</p>
            )}

            <p className="mt-4 text-xs text-gray-400 text-center">
              Powered by Plaid. Your credentials are never stored.
            </p>
          </>
        )}
      </div>
    </div>
  )
}

function StatusDot({ status }: { status: Status }) {
  const colors: Record<Status, string> = {
    checking: 'bg-gray-300 animate-pulse',
    disconnected: 'bg-red-400',
    connecting: 'bg-yellow-400 animate-pulse',
    connected: 'bg-green-400',
    error: 'bg-red-400',
  }
  return <span className={`w-2.5 h-2.5 rounded-full shrink-0 ${colors[status]}`} />
}

function StatusLabel({ status, isOAuthReturn }: { status: Status; isOAuthReturn: boolean }) {
  if (isOAuthReturn && status === 'connecting') {
    return <span className="text-sm text-gray-600">Resuming bank connection…</span>
  }
  const labels: Record<Status, string> = {
    checking: 'Checking connection…',
    disconnected: 'Not connected',
    connecting: 'Connecting…',
    connected: 'Connected',
    error: 'Connection failed',
  }
  return <span className="text-sm text-gray-600">{labels[status]}</span>
}
