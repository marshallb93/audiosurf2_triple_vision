Shader "VertexColorInvertUnlitTintedAddSmooth" {
	Properties {
		_MainTex ("Base (RGB)", 2D) = "white" { }
		_Color ("Tint", Color) = (1,1,1,1)
	}

	SubShader {
		Tags {"Queue" = "Transparent" }
		BindChannels {
			Bind "Vertex", vertex
			Bind "texcoord", texcoord
			Bind "Color", color
		}
		Pass {
			ZWrite Off ColorMask RGB
			Blend One OneMinusSrcColor

			SetTexture [_MainTex] {
				Combine texture * one - primary
			}
			SetTexture [_MainTex] {
				constantColor [_Color]
				Combine previous * constant
			}
		}
	}
} 