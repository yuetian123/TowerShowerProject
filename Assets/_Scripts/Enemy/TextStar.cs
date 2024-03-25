using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class TextStar : MonoBehaviour
{
    private Transform camera;
    private Text text;
    private int state;
    private void Awake()
    {
        text = GetComponent<Text>();
        camera = GameObject.FindGameObjectWithTag("PlayerCamera").transform;
    }
    private void Update()
    {

        if (gameObject.active)
        {
            if (state == 0)
            {
                text.color = Color.Lerp(text.color, Color.clear, 0.02f);
                if (text.color.a < 0.2f)
                {
                    state = 1;
                }
            }
            if (state == 1)
            {
                text.color = Color.Lerp(text.color, Color.white, 0.02f);
                if (text.color.a > 0.9f)
                {
                    state = 0;
                }
            }
        }
        
    }
}
