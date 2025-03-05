import type {ReactNode} from 'react';
import clsx from 'clsx';
import Heading from '@theme/Heading';
import styles from './styles.module.css';

type FeatureItem = {
  title: string;
  Svg: React.ComponentType<React.ComponentProps<'svg'>>;
  description: ReactNode;
};

const FeatureList: FeatureItem[] = [
  {
    title: 'Built for enterprise scalability',
    Svg: require('@site/static/img/enterprise-scalability.svg').default,
    description: (
      <>
<strong>Designed to grow with your business needs.</strong><br/> The architecture efficiently handles increasing transaction volumes and customer bases without performance degradation, ensuring your infrastructure can scale seamlessly as your business expands.
      </>
    ),
  },
  {
    title: 'Supports millions of customer accounts',
    Svg: require('@site/static/img/customer-accounts.svg').default,
    description: (
      <>
<strong>Manage vast customer bases with confidence.</strong><br/>
The Deposit Wallet easily accommodates over a million unique addresses in a single wallet instance, eliminating the need for complex workarounds or multiple wallet management as your user base grows.
      </>
    ),
  },
  {
    title: 'Enhanced security with Bech32 encoding',
    Svg: require('@site/static/img/address-book.svg').default,
    description: (
      <>
<strong>Protect customer funds with advanced address technology.</strong><br/> Bech32 encoding provides built-in error detection that significantly reduces the risk of typos and transcription errors, preventing accidental loss of funds due to address mistakes.
      </>
    ),
  },
  {
    title: 'Optimized for high-volume transactions',
    Svg: require('@site/static/img/cargo-ship.svg').default,
    description: (
      <>
<strong>Process transactions efficiently at scale.</strong><p/> Engineered specifically to handle large UTxO sets and high transaction throughput, the Deposit Wallet maintains consistent performance even during peak operational periods or with expanding transaction histories.
      </>
    ),
  },
  {
    title: 'Official exchange integration support',
    Svg: require('@site/static/img/exchange.svg').default,
    description: (
      <>
<strong>Integrate with confidence using officially supported features.</strong><p/> Unlike unofficial workarounds, the Deposit Wallet provides documented, tested, and supported methods for assigning addresses to customers, ensuring long-term stability for your business operations.
      </>
    ),
  },
  {
    title: 'Merchant-ready infrastructure',
    Svg: require('@site/static/img/merchant.svg').default,
    description: (
      <>
<strong>Accept Cardano payments for your business with ease.</strong><p/> From small online stores to large merchants, the Deposit Wallet provides the infrastructure needed to track customer payments reliably and associate them with the correct customer accounts.
      </>
    ),
  },
];

function Feature({title, Svg, description}: FeatureItem) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className="text--center padding-horiz--md">
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures(): ReactNode {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
