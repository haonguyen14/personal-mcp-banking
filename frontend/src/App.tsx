import { apiEngine } from './engine'
import ConnectPage from './pages/ConnectPage'

export default function App() {
  return <ConnectPage engine={apiEngine} />
}
